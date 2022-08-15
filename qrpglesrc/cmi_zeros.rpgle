      *****************************************************************
     h copyright('(C) Copyright 1996 - 2000 King III Solutions, Inc.  +
     h Rel 4.26 2000-06-23       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ALWNULL(*USRCTL) OPTION(*NODEBUGIO)
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2000 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: CMI_PRDLNK
      **   Type: ILE RPG Program
      **   Desc: Copy I_PRDLNK fields to Core-Mark K_PRDDLNK.
      **
      *****************************************************************
       dcl-f i_prodlnk usage(*update);
       // CMI work file

       dcl-s alpha5     char(1);
       dcl-s alpha6     char(1);
       dcl-s prod4      char(4);
       dcl-s prod5      char(5);

       //===========================================================================================
       // Main calcs
       //===========================================================================================
       //
       setll *start i_prodlnk;
       read i_prodlnk;
       dow not %eof(i_prodlnk);
         alpha5 = *blanks;
         alpha6 = *blanks;
         prod4  = *blanks;
         prod5  = *blanks;

         alpha5 = %subst(frmprod:5:1);
         alpha6 = %subst(frmprod:6:1);

         if alpha5 = *blanks and alpha6 = *blanks;
            prod4 = %subst(frmprod:1:4);
            frmprod = '000000';
            %subst(frmprod:3:%len(prod4)) = prod4;
         else;
             if alpha6 = *blanks;
                prod5 = %subst(frmprod:1:5);
                frmprod = '000000';
                %subst(frmprod:2:%len(prod5)) = prod5;
             endif;
         endif;

         alpha5 = %subst(toprod:5:1);
         alpha6 = %subst(toprod:6:1);

         if alpha5 = *blanks and alpha6 = *blanks;
            prod4 = %subst(toprod:1:4);
            toprod = '000000';
            %subst(toprod:3:%len(prod4)) = prod4;
         else;
             if alpha6 = *blanks;
                prod5 = %subst(toprod:1:5);
                toprod = '000000';
                %subst(toprod:2:%len(prod5)) = prod5;
             endif;
         endif;

         update ri_prodlnk;

         read i_prodlnk;

       enddo;

       *inlr = *on;
