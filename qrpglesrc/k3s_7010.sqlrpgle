      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_7010
      **   Type: ILE RPG Program
      **   Desc: Get purchase order type for creation
      **
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/11/2014.
      **  Remarks. Altered program to utilize SQL select statement to
      **           access sp_potype field in K_SUPLIER record.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 8/18/2014.
      *   Remarks. Replaced chain to K_SUPLIER file with an SQL select
      *            statement.
      *****************************************************************
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d sppotype        s              1
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_7010        PI
     d  comp                          1
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  first_pot                     1
     d*
      /free
       exec sql
         set option commit = *none,
                    datfmt = *iso,
                 closqlcsr = *endactgrp;
       //get po type field from K_SUPLIER
       exec sql
         select sp_potype
           into :sppotype
           from k_suplier
           where sp_comp = :comp and
                 sp_locn = :locn and
                 sp_supl = :supl and
                 sp_suplsub = :suplsub
           fetch first row only;

       //continue if record exists
       if SQLState = SQLStateOk;
       //remember pot
         first_pot = sppotype;
       endif;

       if SQLState = RowNotFound or
          first_pot = *blanks;
            first_pot = 'R';
       endif;

       //-------------------------------------------------------------------

       *inlr = *on;
      /end-free
