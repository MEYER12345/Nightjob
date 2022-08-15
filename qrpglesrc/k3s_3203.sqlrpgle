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
      **   Name: K3S_3203
      **   Type: ILE RPG Program
      **   Desc: Write or update rows in table K_PATHREP and update
      **         those rows with buyr from K_SUPLIER file. Also write
      **         a log row in table K_PATHLOG for each row written or
      **         updated in table K_PATHLOG.
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date.
      *   Remarks.
      *****************************************************************
     d  curr_date      s               d   datfmt(*iso)
     d  curr_time      s               t   timfmt(*iso)
     d  chg_code       s              1a
     d  program        s             10a   inz('K3S_3203')
     d  spbuyr         s              5a
      * --------------------------------------------------- prototype
     d K3S_3203        PR                  EXTPGM('K3S_3203')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3203        PI
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

         exec sql
            select sp_buyr
              into :spbuyr
              from k_suplier
              where sp_comp = :ip_comp and
                    sp_locn = :ip_locn and
                    sp_supl = :ip_supl and
                    sp_suplsub = :ip_suplsub
              fetch first row only;

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
             chg_code = 'A';
             curr_date = %date();
             curr_time= %time();
             exsr insert_pathrep;
             if SQLState = SQLStateOk;
                exsr insert_pathlog;
             endif;
          endif;

         SQLState = SQLStateOk;
       enddo;

       exsr clsipcursor;

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

