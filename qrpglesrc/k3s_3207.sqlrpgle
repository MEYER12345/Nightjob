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
      *****************************************************************
      **
      **   Name: K3S_3207
      **   Type: ILE RPG Program
      **   Desc: Roll up division forecasts and update hubs product
      **         records.
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date.
      *   Remarks.
      *****************************************************************
     d  accumforcast   S              9  2 inz(0)
     d  avgdiff        S              9  2 inz(0)
     d  avgbef         S              9  2 inz(0)
     d  prforcast      S              9  2 inz(0)
     d  prfordevp      S              3  1 inz(0)
     d  chgdesc        S             20a
     d  hubhold        S             10a
     d  prseasonl      S             10a
     d  prusrstat      S              1a
     d  prsysstat      S              1a
     d  prlocn         S              5a
     d  prsupl         S             10a
     d  prsuplsub      S             10a
     d  prprod         S             25a
     d  first_time     S               n   inz('0')
     d  prodhold       S             25a
     d  program        S             10a   inz('K3S_3207')
     d  change_type    S              1a   inz('R')
     d  current_date   S               d
     d  workstn        S             10a   inz('K3SNiteJob')
     d  user           S             10a   inz('KING3     ')
     d  blank1         S              1a
     d  blank5         S              5a
     d  blank10        S             10a
     d  blank25        S             25a
     d  current_time   S               t
     d  zero           S              9  4 inz(0)
     d  codetyp        s              3a   inz('HAS')
     d  codeval        s             20a
     d  hub_locn       s              8a   inz('HUB LOCN')
     d  taflag5        s              1s 0
     d  flag5_count    s              5s 0
     d  bypass_logic   s              1a
     d* --------------------------------------------------- prototype
     d K3S_3207        PR                  EXTPGM('K3S_3207')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3207        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d pathrep_rec   e ds                  extname(k_pathrep)
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       bypass_logic = 'Y';

       exec SQL
          select count(*)      ---Loop thru HAS k_tablcod rows---
             into :flag5_count ---If no ta_flag5 = 1 bypass main logic---
             from k_tablcod
             where ta_comp = :comp and
                   ta_codetyp = :codetyp and
                   ta_flag5 = 1;
       if flag5_count > 0;
          bypass_logic = 'N';
       endif;

       if bypass_logic = 'N';

          exec sql                --Get description for PF_CHGDESC--
             select ta_codeds1
                into :chgdesc
                from k_tablcod
                where ta_comp = :comp and
                      ta_codetyp = 'PFC' and
                      ta_codeval = :change_type  --Roll up averages to a location (hub)--
                fetch first row only;

          exsr dclhrcursor;
          exsr opnhrcursor;

          first_time= *on;
       //--------------------------------------------------------- Main Loop
       //main loop
          Dow SQLSTT = SQLStateOk;
        //------------------------------------
        //fetch k_pathrep row from result set
            exec sql
             fetch next
              from hrcursor
              into :pathrep_rec;

            if SQLState = RowNotFound;
               exsr updthubprod;  //Update rolled up forcast for prods within hub to hub prod rec
               leave;
            endif;

            codeval = *blanks;
            codeval = hub_locn + '  ' + hr_group4;

             exec SQL
                select ta_flag5
                   into :taflag5
                   from k_tablcod
                      where ta_comp = :comp and
                            ta_codetyp = :codetyp and
                            ta_codeval = :codeval
                      fetch first row only;

             if SQLState = SQLStateOk and
                taflag5 = 1;

                if hr_group4 <> hubhold or   //if hub or product changes
                   hr_prod   <> prodhold;
                   if not first_time;
                      exsr updthubprod;  //Update rolled up forcast for prods within hub to hub prod
                      if SQLState = SQLStateOk;
                         exsr insert_prodfor;
                      endif;
                   endif;
                   first_time = *off;
                   hubhold = hr_group4;
                   prodhold = hr_prod;
                   accumforcast = 0;
                endif;

                exec sql
                   select pr_forcast
                      into :prforcast
                      from k_product
                      where pr_comp = :comp and
                            pr_locn = :hr_locn and
                            pr_supl = :hr_supl and
                            pr_suplsub = :hr_suplsub and
                            pr_prod = :hr_prod
                   fetch first row only;
                if SQLState = SQLStateOk;
                   accumforcast += prforcast;
                endif;

                SQLState = SQLStateOk;
             endif;
          enddo;

          exsr clshrcursor;
       endif;
       *inlr = *on;

       begsr dclhrcursor;
       exec sql
        declare hrcursor Cursor
         for
         select *
         from k_pathrep
         where hr_comp = :comp
         order by hr_group4, hr_prod;  ---Sort by product within hub---
       endsr;

       begsr opnhrcursor;
       exec sql
        open hrcursor;
       endsr;

       begsr clshrcursor;
       exec sql
        close hrcursor;
       endsr;

       begsr updthubprod;
          exec SQL
             select pr_forcast, pr_seasonl, pr_usrstat, pr_sysstat, pr_fordevp,
                    pr_locn, pr_supl, pr_suplsub, pr_prod
                into :avgbef, :prseasonl, :prusrstat, :prsysstat, :prfordevp,
                     :prlocn, :prsupl, :prsuplsub, :prprod
                from k_product
                where pr_comp = :comp and
                      pr_locn = :hubhold and
                      pr_prod = :prodhold
                fetch first row only;
          if SQLState = SQLStateOk;
             avgdiff = accumforcast - avgbef;
             exec sql
                 update k_product
                     set pr_forcast = :accumforcast ---Updt hub product rec with rolled up forecast
                          where pr_comp = :comp and
                          pr_locn = :hubhold and
                          pr_prod = :prodhold;
          endif;
       endsr;

       begsr insert_prodfor;
          current_date = %date();
          current_time = %time();
          exec sql
              insert into k_prodfor
                 (pf_comp,
                  pf_locn,
                  pf_supl,
                  pf_suplsub,
                  pf_prod,
                  pf_birth,
                  pf_birthtm,
                  pf_chgtype,
                  pf_chgdesc,
                  pf_avgbef,
                  pf_avgaft,
                  pf_avgdiff,
                  pf_seasbef,
                  pf_seasaft,
                  pf_devpbef,
                  pf_devpaft,
                  pf_statbef,
                  pf_stataft,
                  pf_sysstat,
                  pf_user,
                  pf_workstn,
                  pf_program,
                  pf_frmlocn,
                  pf_frmsupl,
                  pf_frmsub,
                  pf_frmprod,
                  pf_hstmult,
                  pf_addrepl)
              values
                 (:comp,
                  :prlocn,
                  :prsupl,
                  :prsuplsub,
                  :prprod,
                  :current_date,
                  :current_time,
                  :change_type,
                  :chgdesc,
                  :avgbef,
                  :accumforcast,
                  :avgdiff,
                  :prseasonl,
                  :prseasonl,
                  :prfordevp,
                  :prfordevp,
                  :prusrstat,
                  :prusrstat,
                  :prsysstat,
                  :user,
                  :workstn,
                  :program,
                  :blank5,
                  :blank10,
                  :blank10,
                  :blank25,
                  :zero,
                  :blank1);
       endsr;

       /end-free

