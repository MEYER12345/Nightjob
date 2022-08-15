      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO)
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *********************************************************************
      **
      **   Name: K3S_3208
      **   Type: SQLRPGLE program
      **   Desc: Create change type 'J' K_PRODFOR records from 'C', 'H',
      **         'U', 'Y' records to use in Core-Mark Hub and Spoke
      **         programs.
      *********************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 02/28/2022.
      *   Remarks. After K3's testing of 'Hub and Spoke' programs he
      *      requested that I make the following adjustments to this
      *      program:
      *          1) Alter dclpfcursor routine to look for pf_chgtype of
      *             'C' (Customer usage changed average) in addition to
      *             the types it was already looking for which are 'H'
      *             (History reforecasted by user), 'U' (User changed
      *             average), and 'Y' (Copy history).
      *          2) Alter program to insert K_PRODFOR records with
      *             pf_chgtype of 'J' (Hub and Spoke divisional
      *             adjustments) instead of 'X' (External history source).
      *********************************************************************
      * ----------------------------------------------------------
     d prodfor_rec   e ds                  ExtName(k_prodfor)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ----------------------------------------------------------
     d  cmsysdate      s               d
     d  prsupl         s             10
     d  prsuplsub      s             10
     d  hublocn        s             10
     d  codetyp        s              3a   inz('HAS')
     d  codeval        s             20a
     d  hub_locn       s              8a   inz('HUB LOCN')
     d  taflag2        s              1s 0
     d  flag2_count    s              5s 0
     d  bypass_logic   s              1a
      * --------------------------------------------------- prototype
     d K3S_3208        PR                  EXTPGM('K3S_3208')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3208        PI
     d  comp                          1
      * ---------------------------------------------------------- Begin
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endmod;

       bypass_logic = 'Y';

       exec SQL
          select count(*)         ---Loop thru HAS k_tablcod rows---
             into :flag2_count    ---If no ta_flag2 = 1 bypass main logic---
             from k_tablcod
             where ta_comp = :comp and
                   ta_codetyp = :codetyp and
                   ta_flag2 = 1;
       if flag2_count > 0;
          bypass_logic = 'N';
       endif;

       if bypass_logic = 'N';

          exec sql
             select cm_sysdate
                into :cmsysdate   ---host variable to hold date from k_company---
                from k_company
                where cm_comp = :comp ---select k_company row based on parm passed---
                fetch first row only;

          exsr dclpfcursor;  //declare cursor
          exsr opnpfcursor;  //open cursor

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
         dow SQLSTT = SQLStateOk;

        //fetch schedule record to be used for next system date
            exec sql
             fetch next
              from pfcursor
              into :prodfor_rec;

            if SQLState = RowNotFound;
              leave;
            endif;

            exec sql
               select hr_group4       ---hr_group4 holds hub location---
                  into :hublocn
                     from k_pathrep where
                     hr_comp = :comp and
                     hr_locn = :pf_locn and
                     hr_prod = :pf_prod
                     fetch first row only;
            if SQLState = SQLStateOk;

               codeval = *blanks;
               codeval = hub_locn + '  ' + hublocn;

               exec SQL
                  select ta_flag2
                     into :taflag2
                     from k_tablcod
                     where ta_comp = :comp and
                           ta_codetyp = :codetyp and
                           ta_codeval = :codeval
                     fetch first row only;

              if SQLState = SQLStateOk and
                            taflag2 = 1;

                 pf_chgtype = 'J';         //Hub and Spoke divisionsal adjustments
                 pf_chgdesc = 'From Loc=' + pf_locn;
                 pf_locn = hublocn;

                 exec SQL
                    select pr_supl, pr_suplsub ---prsupl is supl signifying hub---
                       into :prsupl, :prsuplsub
                          from k_product where
                          pr_comp = :comp and
                          pr_locn = :hublocn and
                          pr_prod = :pf_prod and
                          pr_altsrce = 0 and
                          pr_deltcnt = 0 and
                          pr_tempory = 0
                          fetch first row only;
                 if SQLState = SQLStateOk;
                    pf_supl = prsupl;
                    pf_suplsub = prsuplsub;
                    exec sql
                       insert into k_prodfor     ---Write 'X' record---
                          values (:prodfor_rec);
                 endif;
              endif;
            endif;
            SQLState = SQLStateOk;
         enddo;

         exsr clspfcursor;
       endif;

       *inlr = *on;

       begsr dclpfcursor;
       exec sql
        declare pfcursor Cursor
         for
         select *  ---select all columns in k_prodfor rows---
         from k_prodfor
         where pf_comp = :comp and
               pf_chgtype in ('C', 'H', 'U', 'Y') and ---these rows created when avg is adjusted---
               pf_birth >= :cmsysdate;
       endsr;

       begsr opnpfcursor;
       exec sql
         open pfcursor;
       endsr;

       begsr clspfcursor;
       exec sql
         close pfcursor;
       endsr;

       /end-free
