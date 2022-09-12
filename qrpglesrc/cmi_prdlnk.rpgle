      *****************************************************************
     h copyright('(C) Copyright 1996 - 2000 King III Solutions, Inc.  +
     h Rel 4.26 2000-06-23       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ALWNULL(*USRCTL) OPTION(*NODEBUGIO)
      ******************************************************************
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
       dcl-f i_prodlnk usage(*input);
       // CMI work file
       //

       dcl-f k_prodlnk usage(*output);
       // Product Links

       dcl-f k_companya usage(*input) keyed;
       // Commpany file

       dcl-s default_date date(*iso) inz(d'0001-01-01');

       //--------------------------- Prototype-----------------------------------------
       Dcl-PR CMI_PRDLNK;
         comp           Char(1);      // Company
       End-PR;
       //--------------------------- Main Procedure Interface -------------------------
       Dcl-PI CMI_PRDLNK;
         comp           Char(1);      // Company
       End-PI;

       //===========================================================================================
       // Main calcs
       //===========================================================================================
       //
       setll *start i_prodlnk;
       read i_prodlnk;
       dow not %eof(i_prodlnk);

         pk_comp    = comp;             //Core-Mark
         pk_type    = 'C';             //Change
         pk_transts = 0;               //0=open, 1=close
         pk_frmlocn = frmlocn;
         pk_frmsupl = frmsupl;
         pk_frmsub  = frmsub;
         pk_frmprod = frmprod;
         pk_frmstat = ' ';
         pk_frmfrez = default_date;    //date field
         pk_frmclr  = 0;
         chain (comp) k_companya;
         if %found(k_companya);
            pk_birth   = cm_sysdate;
            pk_lastupd = cm_sysdate;
         endif;
         pk_to_locn = to_locn;
         pk_to_supl = to_supl;
         pk_to_sub  = to_sub;
         pk_to_prod = to_prod;
         pk_hstmult = 1.00;
         pk_chgtype = 'A';             //'A'=add, 'R'=replace
         pk_dislink = default_date;    //date field
         pk_copycmp = default_date;    //date field

         write rk_prodlnk;

         read i_prodlnk;

       enddo;

       *inlr = *on;
