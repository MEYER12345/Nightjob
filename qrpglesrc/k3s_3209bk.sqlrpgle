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
      **   Name: K3S_3209
      **   Type: SQLRPGLE program
      **   Desc: Create change type 'J' K_PRODFOR records from 'C', 'H',
      **         'U', 'Y' records brought back from ADC (750) and RDC
      **         (520).
      *********************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date.
      *   Remarks.
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
     d K3S_3209        PR                  EXTPGM('K3S_3209')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3209        PI
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

          exsr prcpfADCcursor;    //Process ADC cursor

          exsr prcpfRDCcursor;    //Process RDC cursor
       endif;

       *inlr = *on;


       begsr prcpfADCcursor;  //declare cursor

       exsr dclpfADCcursor;
       exsr opnpfADCcursor;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
       dow SQLSTT = SQLStateOk;

       //fetch schedule record to be used for next system date
           exec sql
             fetch next
              from pfADCcursor
              into :prodfor_rec;

            if SQLState = RowNotFound;
              leave;
            endif;

            exec sql
               select ip_group4       ---hr_group4 holds hub location---
                  into :hublocn
                     from intprod750 where
                     ip_comp = :pf_comp and
                     ip_locn = :pf_locn and
                     ip_prod = :pf_prod
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
                    pf_comp = comp;
                    pf_supl = prsupl;
                    pf_suplsub = prsuplsub;
                    pf_chgtype = 'J';         //Hub and Spoke divisionsal adjustments
                    pf_chgdesc = 'From Loc=' + pf_locn;
                    pf_locn = hublocn;
                    exec sql
                       insert into k_prodfor     ---Write 'J' record---
                          values (:prodfor_rec);
                    if SQLState = SQLStateOk;
                       exec sql
                          delete from prodfor750 ---Delete CHUY PRODFOR750 record---
                             where current of pfADCcursor;
                    endif;
                 else;
                    exec sql
                       insert into prodforerr     ---Write error record---
                          values (:prodfor_rec);
                    if SQLState = SQLStateOk;
                        exec sql
                          delete from prodfor750
                             where current of pfADCcursor;
                    endif;
                 endif;
              endif;
            else;
               exec sql
                  insert into prodforerr     ---Write error record---
                    values (:prodfor_rec);   ---Delete CHUY prodfor750 record---
                    if SQLState = SQLStateOk;
                        exec sql
                          delete from prodfor750
                             where current of pfADCcursor;
                    endif;
            endif;
            SQLState = SQLStateOk;
       enddo;
       exsr clspfADCcursor;

       endsr;

       begsr prcpfRDCcursor;  //declare cursor

       exsr dclpfRDCcursor;
       exsr opnpfRDCcursor;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
       dow SQLSTT = SQLStateOk;

       //fetch schedule record to be used for next system date
           exec sql
             fetch next
              from pfRDCcursor
              into :prodfor_rec;

            if SQLState = RowNotFound;
              leave;
            endif;

            exec sql
               select ip_group4       ---hr_group4 holds hub location---
                  into :hublocn
                     from intprod520 where
                     ip_comp = :pf_comp and
                     ip_locn = :pf_locn and
                     ip_prod = :pf_prod
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
                    pf_comp = comp;
                    pf_supl = prsupl;
                    pf_suplsub = prsuplsub;
                    pf_chgtype = 'J';         //Hub and Spoke divisional adjustments
                    pf_chgdesc = 'From Loc=' + pf_locn;
                    pf_locn = hublocn;

                    exec sql
                       insert into k_prodfor      ---Write 'J' record---
                          values (:prodfor_rec);
                    if SQLState = SQLStateOk;
                        exec sql
                           delete from prodfor520 ---Delete CHUY prodfor520 record---
                              where current of pfRDCcursor;
                    endif;
                 else;
                    exec sql
                       insert into prodforerr     ---Write error record---
                          values (:prodfor_rec);   ---Delete CHUY prodfor520 record---
                    if SQLState = SQLStateOk;
                        exec sql
                          delete from prodfor520
                             where current of pfRDCcursor;
                    endif;
                 endif;
              endif;
            else;
               exec sql
                  insert into prodforerr     ---Write error record---
                    values (:prodfor_rec);   ---Delete CHUY prodfor520 record---
                    if SQLState = SQLStateOk;
                        exec sql
                          delete from prodfor520
                             where current of pfRDCcursor;
                    endif;
            endif;
            SQLState = SQLStateOk;
       enddo;
       exsr clspfRDCcursor;

       endsr;

       begsr dclpfADCcursor;
       exec sql
        declare pfADCcursor Cursor
         for
         select *  ---select all columns in k_prodfor rows---
         from prodfor750
         where pf_chgtype in ('C', 'H', 'U', 'Y') and ---these rows created when avg is adjusted---
               pf_birth >= :cmsysdate;
       endsr;

       begsr opnpfADCcursor;
       exec sql
         open pfADCcursor;
       endsr;

       begsr clspfADCcursor;
       exec sql
         close pfADCcursor;
       endsr;

       begsr dclpfRDCcursor;
       exec sql
        declare pfRDCcursor Cursor
         for
         select *  ---select all columns in k_prodfor rows---
         from prodfor520
         where pf_chgtype in ('C', 'H', 'U', 'Y') and ---these rows created when avg is adjusted---
               pf_birth >= :cmsysdate;
       endsr;

       begsr opnpfRDCcursor;
       exec sql
         open pfRDCcursor;
       endsr;

       begsr clspfRDCcursor;
       exec sql
         close pfRDCcursor;
       endsr;

       /end-free
