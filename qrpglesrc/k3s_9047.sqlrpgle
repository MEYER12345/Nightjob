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
      **   Name: K3S_9047
      **   Type: ILE RPG Program
      **   Desc: Set flag for Period End exceptions control
      **
      *****************************************************************
      **
      **      UDE - User Period End Exceptions
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/01/2014.
      *
      *   Remarks. Changed K3S_9047 to utilize an SQL cursor to read
      *            through K_PRODSEB records for the passed company.
      *            Also, altered program to use an SQL select statement
      *            statement to update the desired K_TABLCOD record if
      *            it exists or use an SQL insert statement to create
      *            the record if it does not already exist.
      *****************************************************************
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9047        PI
     d  comp                          1
     d*
     d cmsysdate       s               d
     d init_date       s               d   inz(d'0001-01-01') datfmt(*iso)
     d
     d
     d*
     d prodseb_rec   e ds                  ExtName(k_prodseb)
     d tablcod_rec   e ds                  ExtName(k_tablcod)

     d SqlStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
       //--------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       exec sql
        Select cm_sysdate
           into :cmsysdate
           from k_company
           where cm_comp = :comp
           fetch first row only;
       //--------------------------------------------declare & open cursor
       exsr dclpbcursor;
       exsr opnpbcursor;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch

       dow SQLState = SqlStateOk;                 //row found
          exec sql
           fetch next
            from pbcursor
            into :prodseb_rec;

          If SQLState = RowNotFound;
             leave;
          endif;

          if pb_reqtype >= 'PE1' AND
             pb_reqtype <= 'PE9' AND
             pb_birth = cmsysdate;

       //------------------------------------------------------- UDE records
       // set 'User period end exceptions' records
             exec sql
              Update k_tablcod
               set ta_flag1 = 0
               where ta_comp = :comp and
                     ta_codetyp = 'UPE' and
                     ta_codeval = :pb_actuser;  //actual user

             if SQLState = RowNotFound;
                exsr inserttcrow;
             endif;
             SQLState = SQLStateOk;
       //-------------------------------------------------------

          endif;
       enddo;

       exsr clspbcursor;               //close cursor
       *inlr = *on;

       begsr dclpbcursor;
       exec sql
        declare pbcursor Cursor
          for
        select *
          from k_prodseb
          where pb_comp = :comp
          order by pb_comp,
                   pb_status,
                   pb_actbuyr,
                   pb_batch,
                   pb_actregn,
                   pb_actlocn,
                   pb_actsupl,
                   pb_actsub;
       endsr;

       begsr opnpbcursor;
       exec sql
        open pbcursor;
        if SQLState <> SQLStateOk;
           exsr clspbcursor;
           exec sql
            open pbcursor;
        endif;
       endsr;

       begsr clspbcursor;
       exec sql
        close pbcursor;
       endsr;

       begsr inserttcrow;
       exec sql
        insert into k_tablcod
               (ta_comp,
                ta_codetyp,
                ta_birth,
                ta_lastupd,
                ta_codeval,
                ta_codeds1,
                ta_codeds2,
                ta_codeds3,
                ta_codeds4,
                ta_codeds5,
                ta_flag1,
                ta_flag2,
                ta_flag3,
                ta_flag4,
                ta_flag5,
                ta_number1,
                ta_number2,
                ta_number3,
                ta_protect,
                ta_sequenc)
       values (:comp,
               'UPE',
               :init_date,
               :init_date,
               :pb_actuser,
               ' ',
               ' ',
               ' ',
               ' ',
               ' ',
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               0,
               ' ');
       endsr;

      /end-free
