#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "MMLtargetDef.h"


struct macro {
    MMLStruct* content;
    int ID;
    struct macro* next;
};
struct macro* MACHEAD;

//Go through the parse tree, substituting implys, octave shifts, macros and such
MMLStruct* processMML(MMLStruct* curr) {
    //Buffer underrun protection
    MMLStruct MMLhead = {0};
    MMLhead.next = curr;
    if (!curr->prev) curr->prev = &MMLhead;
    struct macro macdummy = {NULL, 0, NULL};
    MACHEAD = &macdummy;
    int imply = -1;
    int octave = 4;
    bool onChan = false;
    bool onMacro = false;
    struct macro* thisMacro = NULL;
    int macroSkip = 0;  //Macro's second value on declaration
    MMLStruct* macroTail = NULL;
    for (; curr; curr=curr->next) {
        //Substitute octave shifts
        if (curr->kind == dir_octShift) {
            curr->kind = dir_octave;
            curr->primaryVal = octave + abs(curr->primaryVal)*(curr->isUp?1:-1);
        };
        //Ties don't take an argument
        if (curr->kind == dir_tie) {
                continue;
        }
        //Place implys
        if ((curr->primaryVal == -1 || curr->secondVal == -1) && imply == -1)
            fail(curr->line, curr->column, "no imply before this point");
        if (curr->primaryVal == -1) curr->primaryVal = imply;
        if (curr->secondVal == -1) curr->secondVal = imply;
        //Get implys
        if (curr->kind == dir_imply) {
            imply = curr->primaryVal;
            curr = curr->prev;
            delMMLStruct(curr->next);
            continue;
        }
        //Manage the octave counter
        if (curr->kind == dir_octave) {
            octave = curr->primaryVal;
        }
        if (curr->kind == dir_channel) {
            octave = 4;
            onChan = true;
            onMacro = false;
        }
        //Macro termination
        if (curr->kind == dir_macro) {
            onMacro = false;
        }
        //Macro copying
        if (onMacro) {
            if (!macroSkip) {
                if (!thisMacro->content) {
                    //head
                    thisMacro->content = macroTail = copyMMLStruct(curr);
                } else {
                    //body
                    macroTail = addMMLStruct(macroTail, copyMMLStruct(curr));
                }
            } else {
                macroSkip--;
            }
            //Don't let macros be compiled and sent to output
            curr = curr->prev;
            delMMLStruct(curr->next);
            continue;
        }
        //Macro substitution
        if (curr->kind == dir_macro) {
            if (onChan) {
                //Paste macro
                //Search for it
                thisMacro = MACHEAD;
                for (; thisMacro; thisMacro=thisMacro->next) {
                    if (thisMacro->ID == curr->primaryVal) break;
                }
                if (!thisMacro) {
                    //No such macro
                    fail(curr->line, curr->column, "macro undefined");
                }
                MMLStruct* copier = thisMacro->content;
                //Skip leading directives (macro's second option)
                for (macroSkip = 0; macroSkip < curr->secondVal; macroSkip++) {
                    if (!copier) break;
                    copier = copier->next;
                }
                curr = curr->prev;
                delMMLStruct(curr->next);
                //Copy it in
                for (; copier; copier = copier->next) {
                    curr = addMMLStruct(curr, copyMMLStruct(copier));
                }
            } else {
                //New macro
                //Search for it
                thisMacro = MACHEAD;
                for (; thisMacro->next; thisMacro=thisMacro->next) {
                    if (thisMacro->ID == curr->primaryVal) break;
                }
                //Did we find it?
                if (thisMacro->ID != curr->primaryVal) {
                    //No.
                    thisMacro->next = malloc(sizeof(struct macro));
                    thisMacro = thisMacro->next;
                    thisMacro->ID = curr->primaryVal;
                    thisMacro->next = NULL;
                    thisMacro->content = NULL;
                }
                if (curr->secondVal != -2) {
                    macroSkip = curr->secondVal;
                } else {
                    macroSkip = 0;
                }
                //Remove the contents of content (if any)
                clrMMLStruct(thisMacro->content);
                thisMacro->content = NULL;
                //and copy over the new macro contents
                onMacro = true;
                //Don't let macros be compiled and sent to output
                curr = curr->prev;
                delMMLStruct(curr->next);
            }
        }
    }
    if (MMLhead.next->prev == &MMLhead) MMLhead.next->prev = NULL;
    return MMLhead.next;
}

void clrMMLStruct(MMLStruct* start) {
    while (start) {
        //next might be NULL
        MMLStruct* temp = start->next;
        free(start);
        start = temp;
    };
}

MMLStruct* copyMMLStruct(MMLStruct* src) {
    MMLStruct* result = malloc(sizeof(MMLStruct));
    memcpy(result, src, sizeof(MMLStruct));
    result->prev = result->next = NULL;
    return result;
}

void delMMLStruct(MMLStruct* entry) {
    entry->prev->next = entry->next;
    entry->next->prev = entry->prev;
    free(entry);
}

//Adds into the next field!
MMLStruct* addMMLStruct(MMLStruct* curr, MMLStruct* next) {
    next->prev = curr;
    next->next = curr->next;
    curr->next = next;
    if (next->next) next->next->prev = next;
    return next;
}

