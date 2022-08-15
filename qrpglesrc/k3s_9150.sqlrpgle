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
      **   Name: K3S_9150
      **   Type: ILE RPG Program
      **   Desc: Save Alternate source entries to history file
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/01/2014.
      **  Remarks. Removed OPYQRYF CL statements pertaining to K3S_9150
      **           from K3S_9110CL and left the call to RPG program
      **           K3S_9150 adding &COMP as a passed parameter. Changed                            .
      **           K3S_9150 to utilize embedded SQL to read records for
      **           passed company from file K_INTALTR using a cursor
      **           and write records to file K_HSTALTR using an SQL
      **           insert statement.
      *****************************************************************
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d intaltr_rec   e ds                  ExtName(k_intaltr)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9150        PI
     d  comp                          1

      /free
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //interface for alternate source system
       exsr dcliacursor;
       //exsr clsiacursor;
       exsr opniacursor;

       Dow SQLState = SQLStateOk;

          exec sql
             fetch next
              from iacursor
              into :intaltr_rec;

          If SQLState = RowNotFound;
             leave;
          endif;

          exsr insertintaltr;

       enddo;

       exsr clsiacursor;

       *inlr = *on;

       begsr dcliacursor;
       exec sql
        declare iacursor Cursor
         for
         select *
         from k_intaltr
         where ia_comp = :comp;
       endsr;

       begsr opniacursor;
       exec sql
        open iacursor;
        if SQLState <> SQLStateOk;
           exsr clsiacursor;
           exec sql
            open iacursor;
        endif;
       endsr;

       begsr clsiacursor;
       exec sql
        close iacursor;
       endsr;

       begsr insertintaltr;
       //pending order header file
       exec sql
         insert into k_hstaltr
           (ha_comp,
            ha_locn,
            ha_birth,
            ha_buyr,
            ha_supl,
            ha_suplsub,
            ha_buyralt,
            ha_suplalt,
            ha_prod,
            ha_costeac,
            ha_cost,
            ha_costdiv,
            ha_qtyavil,
            ha_loadcod,
            ha_costreg,
            ha_deal,
            ha_dealeac,
            ha_selectd,
            ha_prefseq,
            ha_restric,
            ha_supldsc,
            ha_suplreb,
            ha_supldtg,
            ha_prodreb,
            ha_altrreb)
         values (:ia_comp,
                 :ia_locn,
                 :ia_birth,
                 :ia_buyr,
                 :ia_supl,
                 :ia_suplsub,
                 :ia_buyralt,
                 :ia_suplalt,
                 :ia_prod,
                 :ia_costeac,
                 :ia_cost,
                 :ia_costdiv,
                 :ia_qtyavil,
                 :ia_loadcod,
                 :ia_costreg,
                 :ia_deal,
                 :ia_dealeac,
                 :ia_selectd,
                 :ia_prefseq,
                 :ia_restric,
                 :ia_supldsc,
                 :ia_suplreb,
                 :ia_supldtg,
                 :ia_prodreb,
                 :ia_altrreb);

       endsr;
      /end-free
