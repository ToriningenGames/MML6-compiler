//Self target
//Allows ouputting MML to MML

#include <stdio.h>
#include <stdbool.h>
#include "MMLtargetDef.h"

int writeMML_self(struct token* curr, FILE* outfile) {
    bool prettyChannel = false; //Newlines after channel numbers
    for (; curr; curr = curr->next) {
        switch (curr->kind) {
            case dir_volume :
            fputc('V', outfile);
            break;
            case dir_sweep :
            fputc('S', outfile);
            break;
            case dir_tempo :
            fputc('T', outfile);
            break;
            case dir_octave :
            fputc('O', outfile);
            break;
            case dir_instrument :
            fputc('I', outfile);
            break;
            case dir_stacatto :
            fputc('H', outfile);
            break;
            case dir_channel :
            fputc('\n', outfile);
            fputc('X', outfile);
            prettyChannel = true;
            break;
            case dir_imply :
            fputc('^', outfile);
            break;
            case dir_macro :
            fputc('\n', outfile);
            fputc('@', outfile);
            break;
            case dir_loop :
            fputc((curr->isaLoopSet)? '[' : ']', outfile);
            break;
            case dir_octShift :
            fputc((curr->isUp)? '>' : '<', outfile);
            break;
            case dir_rest :
            fputc('R', outfile);
            break;
            case dir_label :
            fputc(':', outfile);
            break;
            case dir_note :
            switch (curr->notePitch) {
                case 1 :
                fputc('A', outfile);
                break;
                case 2 :
                fprintf(outfile, "B-");
                break;
                case 3 :
                fputc('B', outfile);
                break;
                case 4 :
                fputc('C', outfile);
                break;
                case 5 :
                fprintf(outfile, "D-");
                break;
                case 6 :
                fputc('D', outfile);
                break;
                case 7 :
                fprintf(outfile, "E-");
                break;
                case 8 :
                fputc('E', outfile);
                break;
                case 9 :
                fputc('F', outfile);
                break;
                case 10 :
                fprintf(outfile, "G-");
                break;
                case 11 :
                fputc('G', outfile);
                break;
                case 12 :
                fprintf(outfile, "A-");
                break;
            }
            break;
            default :
            fputs("# unknown directive", outfile);    //Output comment for unknown inputs
            prettyChannel = true;
            break;
        }
        if (curr->primaryVal != -1) {
            fprintf(outfile, "%i", curr->primaryVal);
        }
        if (curr->secondVal != -2) {
            fputc('.', outfile);
        }
        if (curr->secondVal >= 0) {
            fprintf(outfile, "%i", curr->secondVal);
        }
        fprintf(outfile, prettyChannel?"\n":" ");
        prettyChannel = false;
    }
    return 0;
}
