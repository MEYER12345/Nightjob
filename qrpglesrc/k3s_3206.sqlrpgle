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
      **   Name: K3S_3206
      **   Type: ILE RPG Program
      **   Desc: Write or update rows in table K_PATHREP and update
      **         those rows with buyr from K_SUPLIER file. Also write
      **         a log row in table K_PATHLOG for each row written or
      **         updated in table K_PATHLOG.
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 03/11/2022.
      *   Remarks.  Per K3 I modified program so that if suplier
      *     on an already existing row in K_PATHREP changes in
      *     K_SUPLIER it will update K_PATHREP with new supplier.
      *     It will also update field hr_userbch with buyer user ID
      *     from K_TABLCOD row with code type 'BUY' and corresponding
      *     buyer. In addition, this change will write a log row out
      *     to K_PATHLOG with hl_chgtype of 'C'.
      *****************************************************************
     d  curr_date      s               d   datfmt(*iso)
     d  curr_time      s               t   timfmt(*iso)
     d  chg_code       s              1a
     d  program        s             10a   inz('K3S_3206')
     d  spbuyr         s              5a
     d  codetyp1       s              3a   inz('HAS')
     d  codetyp2       s              3a   inz('BUY')
     d  userID         s             10a
     d  codeval        s             20a
     d  hub_locn       s              8a   inz('HUB LOCN')
     d  taflag1        s              1s 0
     d  flag1_count    s              5s 0
     d  bypass_logic   s              1a
     d  buyr_change    s               n
      * --------------------------------------------------- prototype
     d K3S_3206        PR                  EXTPGM('K3S_3206')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3206        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d intprod_rec   e ds                  extname(k_intprod)
     d pathrep_rec   e ds                  extname(k_pathrep)
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       bypass_logic = 'Y';

       exec SQL
          select count(*)         ---Loop thru HAS k_tablcod rows---
             into :flag1_count    ---If no ta_flag1 = 1 bypass main logic---
             from k_tablcod
             where ta_comp = :comp and
                   ta_codetyp = :codetyp1 and
                   ta_flag1 = 1;
       if flag1_count > 0;
          bypass_logic = 'N';
       endif;

       if bypass_logic = 'N';

          exsr dclipcursor;

          exsr opnipcursor;
       //--------------------------------------------------------- Main Loop
       //main loop
          Dow SQLSTT = SQLStateOk;
        //------------------------------------
        //fetch k_intprod row from result set
            exec sql
             fetch next
              from ipcursor
              into :intprod_rec;

            if SQLState = RowNotFound;
              leave;
            endif;

            codeval = *blanks;
            codeval = hub_locn + '  ' + ip_group4;

            exec SQL
               select ta_flag1
                  into :taflag1
                  from k_tablcod
                     where ta_comp = :comp and
                           ta_codetyp = :codetyp1 and
                           ta_codeval = :codeval
                     fetch first row only;

            if SQLState = SQLStateOk and
               taflag1 = 1;

               exec sql
                  select *
                    into :pathrep_rec
                    from k_pathrep
                    where hr_comp = :ip_comp and
                          hr_locn = :ip_locn and
                          hr_supl = :ip_supl and
                          hr_suplsub = :ip_suplsub and
                          hr_prod = :ip_prod
                    fetch first row only;

               if SQLState = RowNotFound;
                  spbuyr = *blanks;
                  exec sql
                     select sp_buyr
                       into :spbuyr
                       from k_suplier
                       where sp_comp = :ip_comp and
                             sp_locn = :ip_locn and
                             sp_supl = :ip_supl and
                             sp_suplsub = :ip_suplsub
                       fetch first row only;

                  userID = *blanks;
                  exec sql
                      select ta_codeds1
                        into :userID
                        from k_tablcod
                        where ta_comp = :ip_comp and
                              ta_codetyp = :codetyp2 and
                              ta_codeval = :spbuyr
                        fetch first row only;

                  chg_code = 'A';
                  curr_date = %date();
                  curr_time= %time();
                  exsr insert_pathrep;
                  if SQLState = SQLStateOk;
                     exsr insert_pathlog;
                  endif;
               else;
                  if SQLState = SQLStateOk;
                     spbuyr = *blanks;
                     exec sql
                        select sp_buyr
                           into :spbuyr
                        from k_suplier
                        where sp_comp = :ip_comp and
                             sp_locn = :ip_locn and
                             sp_supl = :ip_supl and
                             sp_suplsub = :ip_suplsub
                        fetch first row only;

                     userID = *blanks;
                     exec sql
                        select ta_codeds1
                           into :userID
                           from k_tablcod
                        where ta_comp = :ip_comp and
                              ta_codetyp = :codetyp2 and
                              ta_codeval = :spbuyr
                        fetch first row only;

                     buyr_change = *off;
                     if spbuyr <> hr_buyr;
                        buyr_change = *on;
                     endif;
                     curr_date = %date();
                     curr_time= %time();
                     exec SQL
                        update k_pathrep
                           set hr_lastupd = :curr_date,
                               hr_buyr    = :spbuyr,
                               hr_userbch  = :userID
                           where hr_comp = :ip_comp and
                                 hr_locn = :ip_locn and
                                 hr_supl = :ip_supl and
                                 hr_suplsub = :ip_suplsub and
                                 hr_prod = :ip_prod;
                     if buyr_change = *on;
                        chg_code = 'C';
                        if SQLState = SQLStateOk;
                           exsr insert_pathlog;
                        endif;
                     endif;
                  endif;
               endif;
           endif;
           SQLState = SQLStateOk;
         enddo;

         exsr clsipcursor;
       endif;

       *inlr = *on;

       begsr dclipcursor;
       exec sql
        declare ipcursor Cursor
         for
         select *
         from k_intprod
         where ip_comp = :comp and
               ip_group4 <> '          ' and
               ip_group5 <> '          ';
       endsr;

       begsr opnipcursor;
       exec sql
        open ipcursor;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;

       begsr insert_pathrep;
       exec sql
          insert into k_pathrep
             (hr_comp,
              hr_buyr,
              hr_locn,
              hr_supl,
              hr_suplsub,
              hr_suplusr,
              hr_suplusb,
              hr_prod,
              hr_birth,
              hr_lastupd,
              hr_userbch,
              hr_group4,
              hr_group5,
              hr_usera1,
              hr_usera2,
              hr_usera3)
          values (:ip_comp,
                  :spbuyr,
                  :ip_locn,
                  :ip_supl,
                  :ip_suplsub,
                  :ip_suplusr,
                  :ip_suplusb,
                  :ip_prod,
                  :curr_date,
                  :curr_date,
                  :userID,
                  :ip_group4,
                  :ip_group5,
                  :ip_usera1,
                  :ip_usera2,
                  :ip_usera3);
       endsr;

       begsr insert_pathlog;
       exec sql
          insert into k_pathlog
             (hl_comp,
              hl_buyr,
              hl_locn,
              hl_supl,
              hl_suplsub,
              hl_prod,
              hl_birth,
              hl_birthtm,
              hl_chgtype,
              hl_program,
              hl_group4,
              hl_group5,
              hl_usera1,
              hl_usera2,
              hl_usera3)
          values (:ip_comp,
                  :spbuyr,
                  :ip_locn,
                  :ip_supl,
                  :ip_suplsub,
                  :ip_prod,
                  :curr_date,
                  :curr_time,
                  :chg_code,
                  :program,
                  :ip_group4,
                  :ip_group5,
                  :ip_usera1,
                  :ip_usera2,
                  :ip_usera3);
       endsr;

      /end-free

