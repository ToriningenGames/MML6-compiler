//gb target
//Allows outputting MML to the gameboy engine

//2018-10-31:
    //Empty channels crash program (fixed)
    //Loop indicies aren't actually assigned (fixed)
    //Channel 3 wave isn't assigned (fixed)
//2018-11-2:
    //Trailing imply crashes program
    //Nonzero staccatos crash program (fixed)
//2018-11-3:
    //Whole notes overwrite lengths with the incorrect value (1 instead of 255) (fixed)
    //Loop node length assignments not considering rests (fixed)
//2019-1-19:
    //Added support for labels
    //Loop offsets following the loop's jump point don't consider length directives

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "MMLtargetDef.h"

#define high4(X) ((uint8_t)X)>>4
#define low4(X) ((uint8_t)X)&0x0F

/*
 * Idea: Length blocks
 * Since loops will only come from and go to well-defined places,
 * directives can be blocked together with which lengths they will use.
 * Then, control flow can be analyzed to determine where to put length directive
 * Since al length locations are equivalent, it doesn't matter which block we
 * process first, only the order (in how they relate to one another)
 * Starting from the last block sounds like a good idea,
 * since assignments won't determine later blocks'.
 * The block can also see which lengths will be available to it from previous
 * blocks, and declare where those lengths be stored. Further blocks will need
 * to read this and act upon it.
 * The only lengths that will be available will be those from ALL input sources;
 * there could be many jumps to the beginning label, or natural runin.
 * Thankfully, loop leaps won't interfere, so lengths can be done before,
 * allowing for efficient loop leaps.
 * Since a block may jump to its own beginning, it must consider its own output.
 * This will affect choice of overwriting as well as avaiable lengths.
 * Considering that blocks will only have one proper exit, lookahead can happen.
 * 
 * Or, blocks could encode priority. Each block crates a priority list of their
 * lengths. Then, for a given endpoint, we could generate a master priority list
 * A block could communicate which indicies requested lengths are at,
 * and use this list for overwrite determination.
 * 
 * Proceeding back to front is kind of important, though. Memory usage-wise.
 * How, though? Music is by definition a loop; only the beginning is defined
 * So, our algorithm should be as follows:
 * 
 * Divide input into blocks, with loop jumps and loop labels being the delimiter
 * Determine the used lengths in each block, in order from front to back
 * Determine control flow, by listing which blocks transfer to which.
 * Add an "entry block", containing no lengths, to the first block.
 * Starting with the first block
 * 
 * A given block could have the following:
 *      A list of lengths it itself will use, organized by proximity to front
 *      A list of lengths it uses, organized by proximity to back
 *      A list of lengths it needs to account for
 *      A list of lengths each next block uses, organized by proximity to front
 *      A list of lengths each previous block uses, organized from back
 *      Possibly, a list of index/length pairs previous blocks guarantees output
 *      Possibly, a list of lengths overwritten by a following block
 *      Possibly, a list of index/length pairs required by a following block
 * 
 * Once a block has a length's index and set point known,
 * it should be removed from the list of pending lengths.
 * If a block violates an assumption another block makes,
 * the affected length needs to be re-added to the list.
 * 
 * For a given block with some number of the above, what can we set?
 * 
 * If a parent's priority list is greater than LENCNT and one of our lengths
 * is not in it, we are responsible for setting that length, guaranteed.
 * If our own list is larger than LENCNT,
 * we are responsible for setting some of the values.
 * For every length we overwrite,
 * add it to the overwrite list.
 * If we assume the index of a length no parent has written yet,
 * add to the assumed list.
 * 
 * We can avoid the assumed list, because the ultimate authority is the parent
 * block, which has no luxury to assume anything.
 * The parent block can only be sure to have children, and those children can
 * only be sure to have the parent block, already finished, as a parent.
 * Therefore, we need not consider parents' expectations,
 * since they will already be finished.
 * That statement is completely wrong, but still helps solve a few problems.
 * 
 * If we start with the parent, all lengths can be set, and a definitive length
 * list can be generated for end of block.
 * Child blocks can't be sure of their input, if there are other parents, but
 * a definite list goes a long way in determining what WILL need writing.
 * The parent has better access to what would be better served in overwriting,
 * so a parent's word on what indicies have what lengths has authority over
 * children, so a children's request may be denied.
 * 
 * In order to make sure output lengths have reusability across blocks,
 * a given block needs to ensure a length is the same as any siblings.
 * This only needs to be the same for as far as the length is actually present.
 * For determining this, there needs to be child and parent collections.
 * There needs to be one for checked entrys, and unchecked entrys
 * The current block adds itself to unchecked parents.
 * Then, for an entry in unchecked parents, the current length is looked for.
 * If it not present, the given parent is discarded.
 * Otherwise, all its children are added to unchecked children,
 * if not already present in checked children,
 * and it itself is added to checked parents.
 * Once the unchecked parent queue is empty, the unchecked children are checked
 * If it is empty, addition is done.
 * Otherwise, a child is taken, and all its parents are moved to unchecked,
 * if they are not already in checked.
 * Then, the child is moved to checked.
 * Once the unchecked child queue is empty, the flow moves back to the parents.
 * 
 * It is irrelevant if the children don't use the length, since their children's
 * children may use it, and it could pass through to them.
 * 
 * With this, it's unlikely for a child's assumption to be denied, but a
 * contingency plan is still useful and important.
 * 
 * 
 * Crazy idea, but what if we just follow control flow linearly,
 * and insert directives as needed?
 */

//Given a boolean array, return if exactly one entry is true
bool isOne(bool* X, int len)
{
        int n = 0;
        for (int i = 0; i < len; i++) {
                if (X[i])
                        n++;
        }
        return n == 1;
}

#define inRange(X, Y, Z) (Y <= Z && Y >= X)

#define CHCNT 4         //Does not include control channel 0
#define LENCNT 16
#define LOOPCNT 7
struct gb_entryData {
    struct gb_entryData* prev;
    struct gb_entryData* next;
    int line;
    int column;
    int supp;   //Supplementary data, for out of band info. Use if positive
    uint8_t entry;
    uint8_t bytedata;
};

struct length {
    unsigned int index:4;
    uint8_t size;
};

//Contains a given loop
struct loopNode {
    struct gb_entryData* start; //Where the loop set is
    struct gb_entryData* dest;  //Where the loop goes to
    struct gb_entryData* end;   //Where the loop point is
    struct loopNode** children; //Loops this loop contains
    struct loopNode* parent;    //Loop that contains this loop
    bool indexOpt[8];       //Available indicies based on user reservations
};

struct base {
    struct base* prev;
    struct base* next;
};


struct gb_entryData* HEAD[CHCNT+1];

struct loopNode* addChild(struct loopNode* parent);
void assignAllParents(struct loopNode* curr, int index);
void assignAllChildren(struct loopNode* curr, int index);
void killAllChildren(struct loopNode* curr);
int seekLength(struct gb_entryData* curr, int* lengths);
int insertLength(struct gb_entryData* curr, struct length length);
void* copy(void* src, size_t size);
void* add(void* cur, void* nex);
void* pre(void* head, void* prev);
void rem(void* tar);
void* seekLast(void* start);
struct gb_entryData** check(MMLStruct* curr);
int lengthAnd(int* restrict dest, const int* restrict src, int size);
struct gb_entryData* llcollect(struct gb_entryData* curr, struct loopNode* loopTree);
void lpIndAssn(struct loopNode* curr);
struct gb_entryData* loopLengthMap(struct gb_entryData* curr);
int printMML(struct gb_entryData** channels, FILE* outfile);
int writeMML_gb(MMLStruct* curr, FILE* outfile);


//Creates and returns a new child for a given loopNode
struct loopNode* addChild(struct loopNode* parent) {
    if (parent) {
        int childCount = 0;
        while (parent->children[childCount++]);
        parent->children = realloc(parent->children, (childCount--+1) * sizeof(struct loopNode*));
        parent->children[childCount] = malloc(sizeof(struct loopNode));
        parent->children[childCount+1] = NULL;  //Set end
        parent->children[childCount]->parent = parent;
        parent = parent->children[childCount];  //New parent!
        memcpy(parent->indexOpt, parent->parent->indexOpt, 8);  //Inherit available spots from parent
    } else {
        parent = malloc(sizeof(struct loopNode));
        parent->parent = NULL;
        memset(parent->indexOpt, true, 8);
    }
    parent->children = malloc(sizeof(struct loopNode*));
    parent->children[0] = NULL;
    parent->start = NULL;
    parent->end = NULL;
    return parent;
};

//Mark a given index as unavailable for a loopNode and all its ancestors
void assignAllParents(struct loopNode* curr, int index) {
    while (curr) {
        curr->indexOpt[index] = false;
        curr = curr->parent;
    };
}

//Mark a given index as unavailable for a loopNode and all its descendents
void assignAllChildren(struct loopNode* curr, int index) {
    curr->indexOpt[index] = false;
    for (int i = 0; curr->children[i]; i++)
        assignAllChildren(curr->children[i], index);
}

//Frees memory of every descendent of the given node
void killAllChildren(struct loopNode* curr) {
    for (int i = 0; curr->children[i]; i++) {
        killAllChildren(curr->children[i]);
        free(curr->children[i]);
    }
    free(curr->children);
}

//Given a stream position and a current length list, finds the best index to overwrite
int seekLength(struct gb_entryData* curr, int* lengths) {
    //First check for open indicies
    for (int i = 0; i < LENCNT; i++) {
        if (lengths[i] == -1)
            return i;
    }
    //None open; check for last used
    bool avail[LENCNT];
    memset(avail, true, LENCNT);
    for (; curr; curr = curr->next) {
        //For each directive
        if (inRange(0x10, curr->entry, 0x1F)) {
            //Is a length directive; use this index if available
            if (avail[curr->entry & 0x0F])
                return curr->entry & 0x0F;
        }
        if (inRange(0x30, curr->entry, 0xFF)) {
            //Is a note
            int i = 0;
            for (; i < LENCNT && lengths[i] != curr->bytedata; i++)
                ;
            if (i < LENCNT)
                avail[i] = false;
        } 
        if (isOne(avail, LENCNT)) break;
    }
    //Find the first free length
    int ans;
    for (ans = 0; !avail[ans]; ans++)
        ;
    return ++ans;
}

//Creates a copy of add before this entry
struct gb_entryData* insertDirective(struct gb_entryData* curr, struct gb_entryData* add) {
    struct gb_entryData* prev = malloc(sizeof(struct gb_entryData));
    *prev = *add;
    if (curr) {
        prev->prev = curr->prev;
        curr->prev = prev;
    } else {
        prev->prev = NULL;
    }
    prev->next = curr;
    if (prev->prev)
        prev->prev->next = prev;
    return prev;
}

//Inserts a length directive before this entry
int insertLength(struct gb_entryData* curr, struct length length) {
    struct gb_entryData newEntry = {
        .supp = 0,
        .entry = 0x10 | length.index,
        .bytedata = length.size,
    };
    insertDirective(curr, &newEntry);
    return length.size;
}

//Copies a linked list element
void* copy(void* src, size_t size) {
    struct base* res = malloc(size);
    if (src) {
        memcpy(res, src, size);
    } else {
        memset(res, 0, size);
    }
    res->prev = res->next = NULL;
    return res;
}

//Adds a doubly linked list entry after the current entry, and returns the new entry
void* add(void* cur, void* nex) {
    if (!cur) return nex;
    if (!nex) return cur;
    struct base* curr = cur;
    struct base* next = nex;
    next->next = curr->next;
    next->prev = curr;
    if (next->next)
        next->next->prev = next;
    curr->next = next;
    return next;
}

//Adds an element to the head of a list, and returns that
void* pre(void* head, void* pre) {
    if (!head) return pre;
    if (!pre) return head;
    struct base* curr = head;
    struct base* prev = pre;
    prev->prev = curr->prev;
    prev->next = curr;
    if (prev->prev)
        prev->prev->next = prev;
    curr->prev = prev;
    return pre;
}

//Removes the element from the linked list
void rem(void* tar) {
    if (!tar) return;
    struct base* target = tar;
    if (target->prev)
        target->prev->next = target->next;
    if (target->next)
        target->next->prev = target->prev;
    free(target);
}

//Returns the last token
void* seekLast(void* start) {
    if (!start)
        return start;
    struct base* curr = start;
    for (; curr->next; curr=curr->next);
    return curr;
}

//Returns the first token
void* seekFirst(void* start) {
    if (!start)
        return start;
    struct base* curr = start;
    for (; curr->prev; curr=curr->prev);
    return curr;
}

struct gb_entryData** check(MMLStruct* curr) {
    //Check for null input
    if (!curr) return NULL;
    //Compile each directive, with an alternate note format
    struct gb_entryData dummy = {
        .prev = NULL,
        .next = NULL,
        .line = -1,
        .column = -1,
        .supp = -1,
        .entry = 0,
        .bytedata = 0
    };
    HEAD[0] = copy(&dummy, sizeof(struct gb_entryData));
    HEAD[1] = copy(&dummy, sizeof(struct gb_entryData));
    HEAD[2] = copy(&dummy, sizeof(struct gb_entryData));
    HEAD[3] = copy(&dummy, sizeof(struct gb_entryData));
    HEAD[4] = copy(&dummy, sizeof(struct gb_entryData));
    struct gb_entryData* thisEnt = HEAD[0];
    for (; curr; curr=curr->next) {
        thisEnt = add(thisEnt, malloc(sizeof(struct gb_entryData)));
        thisEnt->supp = -1;
        thisEnt->entry = 0;
        thisEnt->bytedata = 0;
        thisEnt->line = curr->line;
        thisEnt->column = curr->column;
        switch (curr->kind) {
            case dir_rest:
                curr->notePitch = 0;
            case dir_note:
                if (!curr->primaryVal || !curr->secondVal)
                    fail(curr->line, curr->column, "zero-length note");
                if (curr->secondVal == -2) curr->secondVal = 1;
                int totalLen = 256 / curr->primaryVal * curr->secondVal;
                //Add notes until the length is satisfactory
                while (totalLen > 255) {
                    thisEnt->entry = (curr->notePitch + 3)<<4;
                    thisEnt->bytedata = 255;
                    totalLen -= 255;
                    thisEnt = add(thisEnt, malloc(sizeof(struct gb_entryData)));
                    thisEnt->entry = 0x03;  //Special tempo
                    thisEnt->bytedata = 0;
                    thisEnt = add(thisEnt, malloc(sizeof(struct gb_entryData)));
                }
                //Normal note
                thisEnt->entry = (curr->notePitch + 3)<<4;
                thisEnt->bytedata = (uint8_t)totalLen;
                break;
            case dir_volume:
                thisEnt->entry = 0x01;
                if (!inRange(0, curr->primaryVal, 255))
                    fail(curr->line, curr->column, "volume out of range");
                thisEnt->bytedata = curr->primaryVal;
                if (!inRange(-2, curr->secondVal, 15))
                    fail(curr->line, curr->column, "volume supplement out of range");
                if (curr->secondVal != -2) {
                    //Second value split is at bit 4
                    thisEnt->bytedata = curr->primaryVal << 4 | curr->secondVal;
                }
                break;
            case dir_sweep:
                thisEnt->entry = 0x00;
                if (!inRange(0, curr->primaryVal, 255))
                    fail(curr->line, curr->column, "sweep out of range");
                thisEnt->bytedata = curr->primaryVal;
                if (!inRange(-2, curr->secondVal, 15))
                    fail(curr->line, curr->column, "sweep supplement out of range");
                if (curr->secondVal != -2) {
                    //Second value split is at bit 4
                    thisEnt->bytedata = curr->primaryVal << 4 | curr->secondVal;
                }
                break;
            case dir_tempo:
                thisEnt->entry = 0x03;
                if (!inRange(0, curr->primaryVal, 255))
                fail(curr->line, curr->column, "tempo out of range");
                thisEnt->bytedata = curr->primaryVal;
                break;
            case dir_octave:
                thisEnt->entry = 0x20;
                if (!inRange(0, curr->primaryVal, 15))
                fail(curr->line, curr->column, "octave out of range");
                thisEnt->entry |= curr->primaryVal;
                thisEnt->bytedata = 0;
                break;
            case dir_instrument:
                thisEnt->entry = 0x04;
                if (!inRange(0, curr->primaryVal, 3))
                fail(curr->line, curr->column, "instrument out of range");
                if (curr->secondVal == -2) {
                    thisEnt->bytedata = curr->primaryVal;
                } else {
                    if (!inRange(0, curr->primaryVal, 255))
                    fail(curr->line, curr->column, "wave instrument out of range");
                    thisEnt->bytedata = curr->secondVal;
                }
                thisEnt->entry |= curr->primaryVal;
                break;
            case dir_stacatto:
                thisEnt->entry = 0x02;
                if (!inRange(0, curr->primaryVal, 255))
                fail(curr->line, curr->column, "stacatto out of range");
                thisEnt->bytedata = curr->primaryVal;
                break;
            case dir_loop:
                thisEnt->entry = 0x08;
                if (!inRange(-2, curr->secondVal, LOOPCNT-1))
                    fail(curr->line, curr->column, "loop index out of range");
                if (!inRange(0, curr->primaryVal, 127) && curr->isaLoopSet)
                    fail(curr->line, curr->column, "loop count out of range");
                thisEnt->bytedata = curr->primaryVal & 0x7F;
                thisEnt->supp = curr->primaryVal;
                thisEnt->bytedata |= 0x80 * (!curr->isaLoopSet);
                if (curr->secondVal != -2) thisEnt->entry |= curr->secondVal;
                break;
            case dir_channel:
                //Change fill channel
                thisEnt = thisEnt->prev;
                rem(thisEnt->next);
                if (!inRange(0, curr->primaryVal, CHCNT)) {
                    fail(curr->line, curr->column, "invalid channel number");
                }
                thisEnt = HEAD[curr->primaryVal];
                break;
            case dir_label:
                //Label; these are not output to file, but stored for loop use
                thisEnt->entry = 0;
                thisEnt->bytedata = 0;
                thisEnt->supp = curr->primaryVal;
                break;
            default:    //Handle unknown directives gracefully
                warn(curr->line, curr->column, "unknown directive");
                thisEnt = thisEnt->prev;
                rem(thisEnt->next);
                break;
        }
    }
    for (int i = 0; i <= CHCNT; i++) {
        if (HEAD[i] && HEAD[i]->next) {
            HEAD[i] = HEAD[i]->next;
            rem(HEAD[i]->prev);
        } else {
            rem(HEAD[i]);
            HEAD[i] = NULL;
        }
    }
    return HEAD;
}

//Loop once: collect loops
struct gb_entryData* llcollect(struct gb_entryData* curr, struct loopNode* loopTree) {
    struct gb_entryData* start = curr;
    for (; curr; curr = curr->next) {
        //Is loop?
        if (inRange(0x08, curr->entry, 0x0F)) {
            //Is loop end?
            if (curr->bytedata >= 0x80) {
                //yes
                //Annotate loop end
                loopTree->end = curr;
                //Close loop
                loopTree = loopTree->parent;
            } else {
                //no
                //Create new entry (addChild())
                loopTree = addChild(loopTree);
                loopTree->start = curr;
                //Set the destination pointer
                struct gb_entryData* end = curr->next;
                for (; end && loopTree->start->supp != end->supp; end = end->next)
                    ;
                loopTree->dest = end;
            }
        }
    }
    return start;
}

//Assign loop indicies for node and all children
void lpIndAssn(struct loopNode* curr) {
    //Make sure preassigned loop ends match
    assert(curr);
    assert(curr->start);
    assert(curr->end);
    if (curr->start->entry == 8)
        curr->start->entry = curr->end->entry;
    if (curr->end->entry == 8)
        curr->end->entry = curr->start->entry;
    if (curr->start->entry == 8) {
        //Does not have preassigned loop
        uint8_t index = 0;
        //Find first index
        while (!curr->indexOpt[index]) {
            index++;
            assert(index < LOOPCNT);
        }
        curr->start->entry |= index;
        curr->end->entry |= index;
        assignAllChildren(curr, index);
    };
    for (int i = 0; curr->children[i]; i++)
        lpIndAssn(curr->children[i]);
}

//For debugging; spit out the whole tree
void printTree(struct loopNode* head) {
    printf("entry %i\tbytedata %i\tentry %i\tbytedata %i\n", head->start->entry, head->start->bytedata, head->end->entry, head->end->bytedata);
    for (int i = 0; head->children[i]; i++) {
        printTree(head->children[i]);
    };
}

//Sets all lengths
struct gb_entryData* lengthMap(struct gb_entryData* curr) {
    int lengths[LENCNT];
    memset(lengths, -1, LENCNT * sizeof(int));
    struct gb_entryData* start = curr;
    struct loopJump {
        struct gb_entryData* start;
        struct gb_entryData* end;
    }* finLoops = malloc(sizeof(struct loopJump));
    int finLoopssize = 0;
    struct gb_entryData* loopSources[LOOPCNT];
    for (int i = 0; i < LOOPCNT; i++) {
        loopSources[i] = NULL;
    }
    for (; curr; curr = curr->next) {
        //Is loop?
        if (inRange(8, curr->entry, 15)) {
            //Is loop end?
            if (curr->bytedata >= 0x80) {
                //yes
                //Follow the matching loop
                struct loopJump thisLoop = {loopSources[curr->entry & 0x07], curr};
                //If we already followed this, don't do it twice.
                for (int i = finLoopssize-1; i >= 0; i--) {
                    if ((finLoops[i].start == thisLoop.start) && (finLoops[i].end == thisLoop.end)) {
                        //We did this already
                        goto LoopSkip;
                    };
                }
                //We are new to this loop pair
                finLoops = realloc(finLoops, sizeof(struct loopJump) * ++finLoopssize);
                finLoops[finLoopssize-1] = thisLoop;
                //Don't do it if it's NULL
                if (!thisLoop.start) {
                    warn(thisLoop.end->line, thisLoop.end->column, "unmatched loop goto");
                } else {
                    curr = thisLoop.start;
                    //Advance to label
                    for (; curr
                        && !(curr->entry == 0
                        && curr->bytedata == 0
                        && curr->supp == thisLoop.start->bytedata);
                        curr = curr->next)
                        ;
                    if (!curr) {
                        warn(thisLoop.start->line, thisLoop.start->column, "loop set without label");
                        curr = thisLoop.start;
                    };
                }
                LoopSkip: ; //We aren't jumping through this loop
            } else {
                //no
                //Set this loop
                loopSources[curr->entry & 0x07] = curr;
            }
        } else if (curr->entry >= 0x30) {   //Is note?
            //yes
            int lenInd = -1;
            //is length in table?
            for (int i = 0; i < LENCNT; i++) {
                if (lengths[i] == curr->bytedata) {
                    lenInd = i;
                    break;
                }
            }
            if (lenInd == -1) {
                //no
                //find best index (seekLength())
                lenInd = seekLength(curr, lengths);
                //overwrite (insertLength())
                lengths[lenInd] = insertLength(curr, (struct length){lenInd, curr->bytedata});
            }
            //assign length
            curr->entry |= lenInd;
        }
    }
    //Rewind
    for (curr = start; curr->prev; curr = curr->prev);
    return curr;
}

//Sets all loop indicies
struct gb_entryData* loopMap(struct gb_entryData* curr) {
    //Shortcut if no input
    if (!curr) return NULL;
    struct loopNode* dummy = addChild(NULL);
    llcollect(curr, dummy);
    for (int i = 0; dummy->children[i]; i++) {
        lpIndAssn(dummy->children[i]);
    }
    killAllChildren(dummy);
    free(dummy);
    //Rewind
    for (; curr && curr->prev; curr = curr->prev);
    return curr;
}

int printMML(struct gb_entryData** channels, FILE* outfile) {
    //Constant header size
    int offset = (CHCNT+1) * 2;
    //Count bytes in each channel
    for (int ch = 0; ch < CHCNT; ch++) {
        fwrite(&offset, 2, 1, outfile); //Channel start
        for (struct gb_entryData* curr = channels[ch]; curr; curr = curr->next) {
            //Nondirectives don't get written
            if (!curr->entry && !curr->bytedata && curr->supp >= 0)
                continue;
            offset++;
            if (curr->entry < 0x20) offset++;
        }
        if (offset > 0xFFFF) {
            fail(0, 0, "song too huge");
        }
    }
    fwrite(&offset, 2, 1, outfile); //Channel 4 start
    //Header printed
    //Print each channel's contents
    for (int ch = 0; ch < CHCNT+1; ch++) {
        for (struct gb_entryData* curr = channels[ch]; curr; curr = curr->next) {
            //Do not write labels
            if (!curr->entry && !curr->bytedata && curr->supp != -1)
                continue;
            fwrite(&curr->entry, 1, 1, outfile);
            if (curr->entry < 0x20) fwrite(&curr->bytedata, 1, 1, outfile);
        }
    }
    return offset;
}

struct jump {
    struct jump* prev;
    struct jump* next;
    struct gb_entryData* loopset;
    int byteSpan;
};

//Sorts a jump list in order of bytelength (small to large)
struct jump* sortJump(struct jump* list) {
    if (!list)
        return list;
    //Seek to tail of provided list
    list = seekLast(list);
    if (!list->prev) {
        //One element is always sorted
        return list;
    };
    struct jump* sorted = copy(list, sizeof(struct jump));
    //For every element
    while (list->prev) {
        list = list->prev;
        //This is for the tail of the while loop
        rem(list->next);
        //Find out which elements this one goes between
        while (sorted->byteSpan < list->byteSpan && sorted->next)
            sorted = sorted->next;
        //This element goes before the one pointed in sorted
        if (sorted->byteSpan < list->byteSpan) {
            //We lied; it actually went after, at the end of the list
            add(sorted, copy(list, sizeof(struct jump)));
        } else {
            sorted = pre(sorted, copy(list, sizeof(struct jump)));
        }
        //Go to head of sorted list
        sorted = seekFirst(sorted);
    }
    rem(list);
    return sorted;
}


//Converts from loops and labels to loops and offsets
int lenOffsetAdj(struct gb_entryData* curr) {
    //Adding one springboard will affect all jumps that jump over it.
    //Assuming we add them in the order they'd appear in file, their placement
    //can be known when we reach them.
    //Looping index 7 has been reserved for use here
    
    //In order to do this, what do we need?
        //How many bytes a jump is
        //Where the jump is from
        //Where it goes
        //Where any and all springboards are
    
    //There can be more than 7 jumps at any one time!
    //There can actually be an arbitrary number. At some point, though, it's insane
    struct jump* jmpList = NULL;
    for (; curr; curr = curr->next) {
        //Read each directive, acting as appropriate
        //Most just count bytes
        if (!curr->entry && !curr->bytedata && curr->supp != -1) {
            //Label
            //One or more jumps may close
            struct jump* thisJmp = jmpList;
            while (thisJmp) {
                if (thisJmp->loopset->supp == curr->supp) {
                    //Close loop
                    thisJmp->loopset->bytedata = thisJmp->byteSpan;
                    if (thisJmp == jmpList)
                        jmpList = jmpList->next;
                    if (!thisJmp->next) {
                        rem(thisJmp);
                        thisJmp = NULL;
                    } else {
                        thisJmp = thisJmp->next;
                        rem(thisJmp->prev);
                    }
                } else {
                    thisJmp = thisJmp->next;
                }
            }
            //Do not count labels; they are not output
            continue;
        };
        if (inRange(0x08, curr->entry, 0x0F) && curr->bytedata < 0x80) {
            //Loop set
            struct jump* newJmp = malloc(sizeof(struct jump));
            newJmp->next = newJmp->prev = NULL;
            newJmp->loopset = curr;
            newJmp->byteSpan = 0;
            jmpList = pre(jmpList, newJmp);
            jmpList = seekFirst(jmpList);
            for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                thisJmp->byteSpan+=2;
            }
        } else if (inRange(0x00, curr->entry, 0x1F)) {
            //Sweep, Envelope, Stacatto, Tempo, Tone, Length, Loops
            //Count two bytes
            for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                thisJmp->byteSpan+=2;
            }
        } else {
            //Every other directive
            //Count one byte
            for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                thisJmp->byteSpan++;
            }
        }
        
        //Do we need a springboard?
        //When would we need a springboard?
            //When one jump is at n-4 bytes
            //When two jumps are under n-8 bytes
            //When three jumps are under n-12 bytes
            //etc.
        //Place the lowest value first.
        //Interestingly, the list is already sorted. We just need to keep it that way.
        //If any given entry meets the requirements, all previous meet as well, and
        //a springboard is needed
        //If the next directive is two bytes, the threshhold lowers
        
        //Sort the jump list
        //Little to big
        int count1 = 0, count2 = 0;
        for (struct jump* j = jmpList; j; j = j->next)
            count1++;
        jmpList = sortJump(jmpList);
        for (struct jump* j = jmpList; j; j = j->next)
            count2++;
        //Don't drop jumps here
        assert(count1 == count2);
        //Check for quantity of matches, from big to small
        //Get jump count
        int jmpCnt = 1;
        struct jump* jmpChk;
        if ((jmpChk = jmpList)) {
            for (struct jump* jmpEnd = jmpChk; jmpEnd->next; jmpEnd = jmpEnd->next)
                jmpCnt++;
            int threshold = 127 - 4 * (jmpCnt + 1);
            //Place a springboard one byte early if next is not size 1
            if (curr->next)
                threshold -= !inRange(0x20, curr->next->entry, 0xFF);
            for (; jmpChk; jmpChk = jmpChk->next) {
                if (jmpChk->byteSpan >= threshold) {
                    break;
                };
                threshold += 4;
                jmpCnt--;
            }
            if (jmpCnt) {
                //Springboard needed, with jmpCnt components
                //Place after this byte!
                curr = curr->next;
                //Place the guard
                //Setloop
                struct gb_entryData spring = {0};
                spring.entry = 0x0F;
                spring.bytedata = 0x00 | ((jmpCnt + 1) * 4);
                spring.supp = -1;
                insertDirective(curr, &spring);
                //Goto
                spring.bytedata = 0x80;
                spring.supp = -1;
                insertDirective(curr, &spring);
                //Place the payload(s)
                struct jump* sprJmp = jmpList;
                //(which is/are at the end (largest bytelength))
                while (sprJmp->next)
                    sprJmp = sprJmp->next;
                while (jmpCnt) {
                    //Each spring size
                    for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                        thisJmp->byteSpan += 4;
                    }
                    //Finalize previous loop
                    assert(sprJmp->byteSpan < 0x80);
                    sprJmp->loopset->bytedata = sprJmp->byteSpan;
                    //Setloop
                    spring.bytedata = 0x00;
                    spring.supp = sprJmp->loopset->supp;
                    //Edit jump list
                    sprJmp->loopset = insertDirective(curr, &spring);
                    sprJmp->byteSpan = 0;   //This gets adjusted later
                    //Goto
                    spring.bytedata = 0x80;
                    spring.supp = -1;
                    insertDirective(curr, &spring);
                    //Next
                    sprJmp = sprJmp->prev;
                    jmpCnt--;
                }
                //And the guard
                for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                    thisJmp->byteSpan += 4;
                }
                //Be sure to count this byte
                curr = curr->prev;
            };
        };
        //No need for springboards if no active jumps!
    }
    return 0;
}

int writeMML_gb(MMLStruct* curr, FILE* outfile) {
    if (!check(curr)) {
        return -1;
    };
    //Assign loop indicies and length indicies
    for (int ch = 0; ch <= CHCNT; ch++) {
        HEAD[ch] = loopMap(HEAD[ch]);
        HEAD[ch] = lengthMap(HEAD[ch]);
        //Adjust loop offsets to count actual bytes
        lenOffsetAdj(HEAD[ch]);
    };
    //Print!
    printMML(HEAD, outfile);
    return 0;
}
