//c64 target
//Allows outputting MML to the commodore 64 engine

//2024-03-26: Initial
        //Copied from the gb form

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "MMLtargetDef.h"

#define high4(X) ((uint8_t)X)>>4
#define low4(X) ((uint8_t)X)&0x0F


//Given a boolean array, return if exactly one entry is true
static bool isOne(bool* X, int len)
{
        int n = 0;
        for (int i = 0; i < len; i++) {
                if (X[i])
                        n++;
        }
        return n == 1;
}

#define inRange(X, Y, Z) (Y <= Z && Y >= X)

#define CHCNT 3         //Does not include control channel 0
#define LENCNT 16
#define LOOPCNT 16
struct entryData {
    struct entryData* prev;
    struct entryData* next;
    int line;
    int column;
    int supp;   //Supplementary data, for out of band info. Use if positive
    uint8_t entry;
    uint16_t bytedata;
};

struct length {
    unsigned int index:4;
    uint8_t size;
};

//Contains a given loop
struct loopNode {
    struct entryData* start; //Where the loop set is
    struct entryData* dest;  //Where the loop goes to
    struct entryData* end;   //Where the loop point is
    struct loopNode** children; //Loops this loop contains
    struct loopNode* parent;    //Loop that contains this loop
    bool indexOpt[LOOPCNT];       //Available indicies based on user reservations
};

struct base {
    struct base* prev;
    struct base* next;
};


static struct entryData* HEAD[CHCNT+1];
static bool inControl;

static int entryLen(struct entryData *curr);
static struct loopNode* addChild(struct loopNode* parent);
static void assignAllParents(struct loopNode* curr, int index);
static void assignAllChildren(struct loopNode* curr, int index);
static void killAllChildren(struct loopNode* curr);
static int seekLength(struct entryData* curr, int* lengths);
static int insertLength(struct entryData* curr, struct length length);
static void* copy(void* src, size_t size);
static void* add(void* cur, void* nex);
static void* pre(void* head, void* prev);
static void rem(void* tar);
static void* seekLast(void* start);
static struct entryData** check(MMLStruct* curr);
static int lengthAnd(int* restrict dest, const int* restrict src, int size);
static struct entryData* llcollect(struct entryData* curr, struct loopNode* loopTree);
static void lpIndAssn(struct loopNode* curr);
static struct entryData* loopLengthMap(struct entryData* curr);
static int printMML(struct entryData** channels, FILE* outfile);
int writeMML_c64(MMLStruct* curr, FILE* outfile);


//Returns how many bytes this one directive will take
static int entryLen(struct entryData *curr)
{
        if (curr->entry == 0 && curr->bytedata == 0 && curr->supp >= 0) {     //Label
                return 0;
        }
        if (curr->entry >= 0x30) { //Note/rest
                return 1;
        }
        if (curr->entry >= 0x20) { //Loop
                return 2;
        }
        if (curr->entry >= 0x10) { //Length
                return 2;
        }
        if (curr->entry >= 0x08) { //Octave
                return 1;
        }
        if (curr->entry == 0x03) { //Tempo
                return 2;
        }
        if (curr->entry == 0x04) { //Control/Filter Enable
                return 2;
        }
        if (curr->entry == 0x05) { //ADSR/Volume
                return inControl ? 2 : 3;
        }
        if ((curr->entry & 0xFE) == 0x06) { //Waveform/Phase / Filter
                return inControl ? 2 : 3;
        }
        if (curr->entry == 0x02) { //Virbrato/Tremolo
                return 2;
        }
        if (curr->entry == 0x01) { //Sweep
                return 2;
        }
        if (curr->entry == 0x00) { //Tie
                return 1;
        }
        return 0;      //Invalid
}

static int MMLStructLen(MMLStruct *curr)
{
        switch (curr->kind) {
                case dir_note :
                case dir_rest :
                case dir_octave :
                        return 1;
                case dir_loop :
                case dir_control :
                case dir_sweep :
                case dir_wobble :
                        return 2;
                case dir_volume :
                case dir_instrument :
                        return inControl ? 2 : 3;
                case dir_channel :
                case dir_label :
                default :
                        return 0;
                case dir_tempo :
                        return curr->primaryVal ? 2 : 1;
        }
}

//Creates and returns a new child for a given loopNode
static struct loopNode* addChild(struct loopNode* parent) {
    if (parent) {
        int childCount = 0;
        while (parent->children[childCount++]);
        parent->children = realloc(parent->children, (childCount--+1) * sizeof(struct loopNode*));
        parent->children[childCount] = malloc(sizeof(struct loopNode));
        parent->children[childCount+1] = NULL;  //Set end
        parent->children[childCount]->parent = parent;
        parent = parent->children[childCount];  //New parent!
        memcpy(parent->indexOpt, parent->parent->indexOpt, LOOPCNT);  //Inherit available spots from parent
    } else {
        parent = malloc(sizeof(struct loopNode));
        parent->parent = NULL;
        memset(parent->indexOpt, true, LOOPCNT);
    }
    parent->children = malloc(sizeof(struct loopNode*));
    parent->children[0] = NULL;
    parent->start = NULL;
    parent->end = NULL;
    return parent;
};

//Mark a given index as unavailable for a loopNode and all its ancestors
static void assignAllParents(struct loopNode* curr, int index) {
    while (curr) {
        curr->indexOpt[index] = false;
        curr = curr->parent;
    };
}

//Mark a given index as unavailable for a loopNode and all its descendents
static void assignAllChildren(struct loopNode* curr, int index) {
    curr->indexOpt[index] = false;
    for (int i = 0; curr->children[i]; i++)
        assignAllChildren(curr->children[i], index);
}

//Frees memory of every descendent of the given node
static void killAllChildren(struct loopNode* curr) {
    for (int i = 0; curr->children[i]; i++) {
        killAllChildren(curr->children[i]);
        free(curr->children[i]);
    }
    free(curr->children);
}

//Given a stream position and a current length list, finds the best index to overwrite
static int seekLength(struct entryData* curr, int* lengths) {
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
static struct entryData* insertDirective(struct entryData* curr, struct entryData* add) {
    struct entryData* prev = malloc(sizeof(struct entryData));
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
static int insertLength(struct entryData* curr, struct length length) {
    struct entryData newEntry = {
        .supp = 0,
        .entry = 0x10 | length.index,
        .bytedata = length.size,
    };
    insertDirective(curr, &newEntry);
    return length.size;
}

//Copies a linked list element
static void* copy(void* src, size_t size) {
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
static void* add(void* cur, void* nex) {
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
static void* pre(void* head, void* pre) {
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
static void rem(void* tar) {
    if (!tar) return;
    struct base* target = tar;
    if (target->prev)
        target->prev->next = target->next;
    if (target->next)
        target->next->prev = target->prev;
    free(target);
}

//Returns the last token
static void* seekLast(void* start) {
    if (!start)
        return start;
    struct base* curr = start;
    for (; curr->next; curr=curr->next);
    return curr;
}

//Returns the first token
static void* seekFirst(void* start) {
    if (!start)
        return start;
    struct base* curr = start;
    for (; curr->prev; curr=curr->prev);
    return curr;
}

static struct entryData** check(MMLStruct* curr) {
    //Check for null input
    if (!curr) return NULL;
    //Compile each directive, with an alternate note format
    struct entryData dummy = {
        .prev = NULL,
        .next = NULL,
        .line = -1,
        .column = -1,
        .supp = -1,
        .entry = 0,
        .bytedata = 0
    };
    for (int i = 0; i < CHCNT + 1; i++) {
            HEAD[i] = copy(&dummy, sizeof(struct entryData));
    }
    struct entryData* thisEnt = HEAD[0];
    for (; curr; curr=curr->next) {
        thisEnt = add(thisEnt, malloc(sizeof(struct entryData)));
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
                //C64 prefers ties to rests; accommodate
                if (totalLen > 256) {
                    //First note
                    thisEnt->entry = (curr->notePitch + 3)<<4;
                    thisEnt->bytedata = 0;
                    totalLen -= 256;
                    thisEnt = add(thisEnt, malloc(sizeof(struct entryData)));
                    thisEnt->entry = 0x00;  //Tie
                    thisEnt->bytedata = -1;
                    thisEnt = add(thisEnt, malloc(sizeof(struct entryData)));
                    //Subsequents are rests
                    //Add notes until the length is satisfactory
                    while (totalLen > 256) {
                        thisEnt->entry = 0x30;
                        thisEnt->bytedata = 0;
                        totalLen -= 256;
                        thisEnt = add(thisEnt, malloc(sizeof(struct entryData)));
                        thisEnt->entry = 0x00;  //Tie
                        thisEnt->bytedata = -1;
                        thisEnt = add(thisEnt, malloc(sizeof(struct entryData)));
                    }
                    //One last note
                    thisEnt->entry = 0x30;
                    thisEnt->bytedata = (uint8_t)totalLen;
                } else {
                    //Normal note
                    thisEnt->entry = (curr->notePitch + 3)<<4;
                    thisEnt->bytedata = (uint8_t)totalLen;
                }
                //Normal note
                break;
            case dir_volume:
                thisEnt->entry = 0x05;
                if (inControl) {
                    if (!inRange(0, curr->primaryVal, 15))
                        fail(curr->line, curr->column, "Volume out of range");
                } else {
                    if (!inRange(0, curr->primaryVal, 0xFFFF))
                        fail(curr->line, curr->column, "ADSR out of range");
                }
                thisEnt->bytedata = curr->primaryVal;
                break;
            case dir_sweep:
                thisEnt->entry = 0x01;
                if (!inRange(0, curr->primaryVal, 255))
                    fail(curr->line, curr->column, "sweep out of range");
                thisEnt->bytedata = curr->primaryVal;
                if (!curr->isUp) {
                    thisEnt->bytedata *= -1;
                }
                break;
            case dir_tempo:
                if (!curr->primaryVal) {
                    //Actually a tie
                    if (inControl)
                        fail(curr->line, curr->column, "Tie within control channel");
                    thisEnt->entry = 0;
                    break;
                }
                thisEnt->entry = 0x03;
                if (!inRange(0, curr->primaryVal, 255))
                fail(curr->line, curr->column, "tempo out of range");
                thisEnt->bytedata = curr->primaryVal;
                break;
            case dir_octave:
                thisEnt->entry = 0x08;
                if (!inRange(0, curr->primaryVal, 7))
                fail(curr->line, curr->column, "octave out of range");
                thisEnt->entry |= curr->primaryVal;
                thisEnt->bytedata = 0;
                break;
            case dir_instrument:
                if (inControl) {
                    if (!inRange(-2, curr->secondVal, 15))
                        fail(curr->line, curr->column, "resonance out of range");
                    if (!inRange(0, curr->primaryVal, 127))
                        fail(curr->line, curr->column, "invalid filter");
                    if (curr->secondVal >= 0 && curr->primaryVal > 7)
                        fail(curr->line, curr->column, "invalid filter");
                    thisEnt->entry = 0x05;
                    thisEnt->bytedata = curr->secondVal >= 0 ? curr->secondVal : 0;
                    thisEnt->bytedata |= curr->primaryVal << (curr->secondVal >= 0 ? 4 : 0);
                } else {
                    if (curr->secondVal > 0x0FFF)
                        fail(curr->line, curr->column, "invalid phase");
                    if (!inRange(0, curr->primaryVal, 15))
                        fail(curr->line, curr->column, "invalid wave spec");
                    thisEnt->entry = 0x06 + !!(curr->isUp);
                    thisEnt->bytedata = curr->secondVal >= 0 ? curr->secondVal : 0x0800;
                    thisEnt->bytedata |= (curr->primaryVal & 0x0F) << 12;
                }
                break;
            case dir_wobble:
                if (!inRange(0, curr->primaryVal, 0xFF))
                    fail(curr->line, curr->column, "wobble out of range");
                if (!inRange(-2, curr->secondVal, 0x0F))
                    fail(curr->line, curr->column, "wobble out of range");
                thisEnt->entry = 0x02;
                thisEnt->bytedata = curr->secondVal < 0 ? 0 : curr->secondVal;
                thisEnt->bytedata |= curr->primaryVal << (curr->secondVal < 0 ? 0 : 4);
                break;
            case dir_control:
                if (!inRange(0, curr->primaryVal, 0x0F))
                    fail(curr->line, curr->column, "control out of range");
                thisEnt->entry = 0x04;
                thisEnt->bytedata = curr->primaryVal;
                break;
            case dir_loop:
                thisEnt->entry = 0x20;
                if (!inRange(-2, curr->secondVal, LOOPCNT-1))
                    fail(curr->line, curr->column, "loop index out of range");
                if (!inRange(0, curr->primaryVal, 127) && curr->isaLoopSet)
                    fail(curr->line, curr->column, "loop count out of range");
                thisEnt->bytedata = curr->primaryVal & 0x7F;
                thisEnt->supp = curr->primaryVal;
                thisEnt->bytedata |= 0x80 * (!curr->isaLoopSet);
                if (curr->secondVal >= 0) thisEnt->entry |= curr->secondVal;
                break;
            case dir_channel:
                //Change fill channel
                thisEnt = thisEnt->prev;
                rem(thisEnt->next);
                if (!inRange(0, curr->primaryVal, CHCNT))
                    fail(curr->line, curr->column, "invalid channel number");
                thisEnt = HEAD[curr->primaryVal];
                //Append to channel
                while (thisEnt->next)
                    thisEnt = thisEnt->next;
                inControl = !curr->primaryVal;
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
static struct entryData* llcollect(struct entryData* curr, struct loopNode* loopTree) {
    struct entryData* start = curr;
    for (; curr; curr = curr->next) {
        //Is loop?
        if (inRange(0x20, curr->entry, 0x2F)) {
            //Is loop end?
            if (curr->bytedata & 0x80) {
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
                struct entryData* end = curr->next;
                for (; end && loopTree->start->supp != end->supp; end = end->next)
                    ;
                loopTree->dest = end;
            }
        }
    }
    return start;
}

//Assign loop indicies for node and all children
static void lpIndAssn(struct loopNode* curr) {
    //Make sure preassigned loop ends match
    assert(curr);
    assert(curr->start);
    assert(curr->end);
    if (curr->start->entry == 0x20)
        curr->start->entry = curr->end->entry;
    if (curr->end->entry == 0x20)
        curr->end->entry = curr->start->entry;
    if (curr->start->entry == 0x20) {
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
static void printTree(struct loopNode* head) {
    printf("entry %i\tbytedata %i\tentry %i\tbytedata %i\n", head->start->entry, head->start->bytedata, head->end->entry, head->end->bytedata);
    for (int i = 0; head->children[i]; i++) {
        printTree(head->children[i]);
    };
}

//Sets all lengths
static struct entryData* lengthMap(struct entryData* curr) {
    int lengths[LENCNT];
    memset(lengths, -1, LENCNT * sizeof(int));
    struct entryData* start = curr;
    struct loopJump {
        struct entryData* start;
        struct entryData* end;
    }* finLoops = malloc(sizeof(struct loopJump));
    int finLoopssize = 0;
    struct entryData* loopSources[LOOPCNT];
    for (int i = 0; i < LOOPCNT; i++) {
        loopSources[i] = NULL;
    }
    for (; curr; curr = curr->next) {
        //Is loop?
        if (inRange(0x20, curr->entry, 0x2F)) {
            //Is loop end?
            if (curr->bytedata >= 0x80) {
                //yes
                //Follow the matching loop
                struct loopJump thisLoop = {loopSources[curr->entry & 0x0F], curr};
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
                loopSources[curr->entry & 0x0F] = curr;
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
static struct entryData* loopMap(struct entryData* curr) {
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

static int printMML(struct entryData** channels, FILE* outfile) {
    //Constant header size
    int offset = (CHCNT+1) * 2;
    //Count bytes in each channel
    for (int ch = 0; ch <= CHCNT; ch++) {
        inControl = !ch;
        fwrite(&offset, 2, 1, outfile); //Channel start
        for (struct entryData* curr = channels[ch]; curr; curr = curr->next) {
            offset += entryLen(curr);
        }
        if (offset > 0xFFFF) {
            fail(0, 0, "song too huge");
        }
    }
    //Header printed
    //Print each channel's contents
    for (int ch = 0; ch <= CHCNT; ch++) {
        inControl = !ch;
        for (struct entryData* curr = channels[ch]; curr; curr = curr->next) {
            //Do not write labels
            if (!curr->entry && !curr->bytedata && curr->supp != -1)
                continue;
            fwrite(&curr->entry, 1, 1, outfile);
            if (entryLen(curr) == 3) fwrite(((char*)&curr->bytedata)+1, 1, 1, outfile);
            if (entryLen(curr) >= 2) fwrite(&curr->bytedata, 1, 1, outfile);
        }
    }
    return offset;
}

struct jump {
    struct jump* prev;
    struct jump* next;
    struct entryData* loopset;
    int byteSpan;
};

//Sorts a jump list in order of bytelength (small to large)
static struct jump* sortJump(struct jump* list) {
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
static int lenOffsetAdj(struct entryData* curr) {
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
        if (inRange(0x20, curr->entry, 0x2F) && curr->bytedata < 0x80) {
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
        } else {
            //Every other directive
            //Count bytes
            for (struct jump* thisJmp = jmpList; thisJmp; thisJmp = thisJmp->next) {
                thisJmp->byteSpan += entryLen(curr);
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
                threshold -= entryLen(curr)-1;
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
                struct entryData spring = {0};
                spring.entry = 0x20;
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

int writeMML_c64(MMLStruct* curr, FILE* outfile) {
    if (!check(curr)) {
        return -1;
    };
    //Assign loop indicies and length indicies
    for (int ch = 0; ch <= CHCNT; ch++) {
        if (!HEAD[ch]) continue;
        inControl = !ch;
        HEAD[ch] = loopMap(HEAD[ch]);
        HEAD[ch] = lengthMap(HEAD[ch]);
        //Adjust loop offsets to count actual bytes
        lenOffsetAdj(HEAD[ch]);
    };
    //Print!
    printMML(HEAD, outfile);
    return 0;
}
