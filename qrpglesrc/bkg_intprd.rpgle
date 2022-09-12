      *****************************************************************
     h copyright('(C) Copyright 1996 - 2000 King III Solutions, Inc.  +
     h Rel 4.26 2000-06-23       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*YES) ALWNULL(*USRCTL) OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2000 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      ******************************************************************
      **
      **   Name: BKG_INTPRD
      **   Type: ILE RPG Program
      **   Desc: Copy I_INTPROD fields to Bottle King K_INTPROD.
      **
      *****************************************************************
       dcl-f i_intprod usage(*input);
       // BKG work file

       dcl-f k_intprod usage(*output);
       // products interface file

       //===========================================================================================
       // Main calcs
       //===========================================================================================
       //
       setll *start i_intprod;
       read i_intprod;
       dow not %eof(i_intprod);

         ip_comp    = comp;
         ip_locn    = locn;
         ip_birth   = birth;
         ip_lastupd = lastupd;
         ip_supl    = supl;
         ip_suplsub = suplsub;
         ip_suplusr = suplusr;
         ip_suplusb = suplusb;
         ip_prod    = prod;
         ip_prodseq = prodseq;
         ip_desc1   = desc1;
         ip_desc2   = desc2;
         ip_mfg     = mfg;
         ip_ndc_upc = ndc_upc;
         ip_uom     = uom;
         ip_packsiz = packsiz;
         ip_tihi    = tihi;
         ip_status  = status;
         ip_minqty  = minqty;
         ip_buymult = buymult;
         ip_group1  = group1;
         ip_group2  = group2;
         ip_group3  = group3;
         ip_group4  = %subst(group4:1:10);
         ip_group5  = %subst(group4:11:10);
         ip_whslocn = whslocn;
         ip_costreg = costreg;
         ip_costdiv = costdiv;
         ip_sales   = sales;
         ip_saleslw = saleslw;
         ip_qtyohnd = qtyohnd;
         ip_qtyoord = qtyoord;
         ip_qtyback = qtyback;
         ip_weight  = weight;
         ip_weightd = weightd;
         ip_volume  = 0;
         ip_volumed = volumed;
         ip_dlysale = dlysale;
         ip_dlyouts = dlyouts;
         ip_trnoord = trnoord;
         ip_altoord = altoord;
         ip_forctyp = 2;          //Default to 12 monthly
         ip_convpak = convpak;
         ip_purincr = purincr;
         ip_contflg = contflg;
         ip_rebate  = rebate;
         ip_procalt = procalt;
         ip_mfgout  = mfgout;
         ip_dlytyp1 = dlytyp1;
         ip_dlytyp2 = dlytyp2;
         ip_dlytyp3 = dlytyp3;
         ip_dlytyp4 = dlytyp4;
         ip_dlytyp5 = dlytyp5;
         ip_dlytyp6 = dlytyp6;
         ip_dlytyp7 = dlytyp7;
         ip_dlytyp8 = dlytyp8;
         ip_dlytyp9 = dlytyp9;
         ip_carcoun = carcoun;
         ip_usera1  = usera1;
         ip_usera2  = usera2;
         ip_usera3  = usera3;
         ip_usern1  = usern1;
         ip_usern2  = usern2;
         ip_usern3  = usern3;
         ip_disothr = disothr;
         ip_disunt7 = disunt7;
         ip_disunt8 = disunt8;
         ip_disunt9 = disunt9;

         write rk_intprod;

         read i_intprod;

       enddo;

       *inlr = *on;
