      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_8530
      **   Type: ILE RPG Program
      **   Desc: Update K_INTPROD with transfer supplier information
      **
      *****************************************************************
      **
      **  This program is used to update the k_intprod file by
      **  changing the regular supplier ID to a transfer supplier ID.
      **  This program is part of the night job, and would run before
      **  K3S_9010.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/19/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_INTPROD.  Also, changed it to
      *            use SQL select statements to access K_TRANSPL,
      *            K_TRANCEN, K_TRANCPY, and K_TRANPRD files.  In
      *            addition, added SQL insert statement to add
      *            K_TRANCPY record if one does not already exist.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
     d w_count         s              5  0                                      count
     d copied          s             10                                         copied date
     d #locn           s                   like(ip_locn)                        location
     d #supl           s                   like(ip_supl)                        supplier
     d #suplsub        s                   like(ip_suplsub)                     supplier sub
     d #prod           s                   like(ip_prod)                        product
     d logtype         s              1
     d user            s             10
     d program         s             10
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)

     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')

      * ----------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_8530        PI
     d  comp                          1
      * -------------------------------------- Likerec Statements
     d intprod_rec   e ds                  ExtName(k_intprod)
     d transpl_rec   e ds                  ExtName(k_transpl)
     d tranprd_rec   e ds                  ExtName(k_tranprd)
     d trancen_rec   e ds                  ExtName(k_trancen)
     d trancpy_rec   e ds                  ExtName(k_trancpy)
      * ------------------------------------------------------- Once Routine
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //call module to retrieve timestamp
         callp K3S_Retrieve_Timestamp(time_stamp);

       //----------------------------------------------------- Read Products
       exsr dclipcursor;
       //exsr clsipcursor;
       exsr opnipcursor;

       //--------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //fetch first product interface row
          exec sql
           fetch next
            from ipcursor
            into :intprod_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          #locn = ip_locn;
          #supl = ip_supl;
          #suplsub = ip_suplsub;
          #prod = ip_prod;
       //----------------------------------------------------- supplier test
       //Find supplier and update k_intprod
          Exec sql
           Select *
             into :transpl_rec
             from k_transpl
             where ts_comp = :ip_comp and
                   ts_locn = :ip_locn and
                   ts_supl = :ip_supl and
                   ts_suplsub = :ip_suplsub
                   fetch first row only;

       //   supplier is set up in transfer supplier file
          If SQLState = SqlStateOk;
            exsr updintprodts;

            exec sql
               fetch current from ipcursor into :intprod_rec;

          //generate copy history log record if new

            Exec sql
             Select *
             into :trancen_rec
             from k_trancen
             where tc_comp = :ip_comp and
                   tc_locnto = :ip_locn and
                   tc_supl = :ip_supl and
                   tc_suplsub = :ip_suplsub
                   fetch first row only;

            If SQLState = SQLStateOk;
       //   transfer system central record exists

              Exec sql
               Select Count(*)
                into :w_count
                from k_trancpy
                where th_comp = :ip_comp and
                      th_locnfrm = :tc_locnfrm and
                      th_locnto = :tc_locnto and
                      th_prod = :ip_prod;

       //   transfer system copy history record does not exist
              if w_count = 0 and ts_cpydemd = 1;
                 th_comp  = ip_comp;
                 th_locnfrm = tc_locnfrm;
                 th_locnto = tc_locnto;
                 th_rsupl = ip_suplusr;         //Regular supplier
                 th_rsuplsb = ip_suplusb;       //Regular sub supplier
                 th_tsupl = tc_supl;            //Transfer supplier
                 th_tsuplsb = tc_suplsub;       //Transfer sub supplier
                 th_prod = ip_prod;
                 th_birth = %date(%subst(%char(time_stamp):1:10):*ISO);
                 th_lastupd = %date(%subst(%char(time_stamp):1:10):*ISO);
                 clear th_copied;

                 exsr insert_trancpy;

                 exsr $_log_it;

              endif;
            else;
               if SQLState = RowNotFound;
                  SQLState = SQLStateOk;
               endif;
            endif;

          else;
       //----------------------------------------------------- product  test

       //transfer system product link
       //prime key list for product
            Exec sql
            Select *
             into :tranprd_rec
             from k_tranprd
             where tp_comp = :ip_comp and
                   tp_locn = :ip_locn and
                   tp_prod = :ip_prod
                   fetch first row only;

       //   product is set up in transfer supplier file
            If SQLState = SqlStateOk;
              exsr updintprodtp;

            exec sql
               fetch current from ipcursor into :intprod_rec;

       //   generate copy history log record if new

              Exec sql
               Select *
                into :trancen_rec
                from k_trancen
                where tc_comp = :ip_comp and
                      tc_locnto = :ip_locn and
                      tc_supl = :ip_supl and
                      tc_suplsub = :ip_suplsub
                      fetch first row only;

              If SQLState = SqlStateOk;
       //    transfer system central record exists
                Exec sql
                 Select Count(*)
                  into :w_count
                  from k_trancpy
                  where th_comp = :ip_comp and
                        th_locnfrm = :tc_locnfrm and
                        th_locnto = :tc_locnto and
                        th_prod = :ip_prod;
       //   transfer system copy history record does not exist
                if w_count = 0 and tp_cpydemd = 1;

                    th_comp = ip_comp;
                    th_locnfrm = tc_locnfrm;
                    th_locnto = tc_locnto;
                    th_rsupl = ip_suplusr;     //Regular supplier
                    th_rsuplsb = ip_suplusb;   //Regular sub supplier
                    th_tsupl = tc_supl;        //Transfer supplier
                    th_tsuplsb = tc_suplsub;   //Transfer sub supplier
                    th_prod = ip_prod;
                    th_birth = %date(%subst(%char(time_stamp):1:10):*ISO);
                    th_lastupd = %date(%subst(%char(time_stamp):1:10):*ISO);
                    clear th_copied;

                    exsr insert_trancpy;

                    exsr $_log_it;

                endif;
              else;
                 if SQLState = RowNotFound;
                    SQLState = SQLStateOk;
                 endif;
              endif;
            else;
               if SQLState = RowNotFound;
                  SQLState = SQLStateOk;
               endif;
            endif;

       // first if
          endif;

       //fetch product interface rows end of loop
         if SQLState = RowNotFound;
           SQLState = SQLStateOk;
         endif;
       enddo;
       //-------------------------------------------------- End of Main Loop

       exsr clsipcursor;

       //finished, set on LR
         *inlr = *on;

       begsr dclipcursor;
       exec sql
        declare ipcursor sensitive scroll cursor
         for
         select *
         from k_intprod
         where ip_comp = :comp
         for update of ip_supl,
                       ip_suplsub;
       endsr;

       begsr opnipcursor;
       exec sql
        open ipcursor;
        if SQLState <> SQLStateOk;
           exsr clsipcursor;
           exec sql
            open ipcursor;
        endif;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;

       begsr updintprodts;
       exec sql
        update k_intprod
         Set ip_supl = :ts_tsupl,
             ip_suplsub = :ts_tsuplsb
         where current of ipcursor;
       endsr;

       begsr updintprodtp;
       exec sql
        update k_intprod
         Set ip_supl = :tp_tsupl,
             ip_suplsub = :tp_tsuplsb
         where current of ipcursor;
       endsr;

       begsr insert_trancpy;
       Exec sql
        insert into k_trancpy
                    (th_comp,
                     th_locnfrm,
                     th_locnto,
                     th_rsupl,
                     th_rsuplsb,
                     th_tsupl,
                     th_tsuplsb,
                     th_prod,
                     th_birth,
                     th_lastupd,
                     th_copied)
        values      (:th_comp,
                     :th_locnfrm,
                     :th_locnto,
                     :th_rsupl,
                     :th_rsuplsb,
                     :th_tsupl,
                     :th_tsuplsb,
                     :th_prod,
                     :th_birth,
                     :th_lastupd,
                     :th_copied);

       endsr;

       begsr $_log_it;

       logtype = 'A';
       user = 'NIGHT JOB';
       program = 'K3S_8530';
       callp K3S_8557(th_comp:
                      logtype:
                      user:
                      program:
                      time_stamp:
                      th_locnfrm:
                      th_locnto:
                      th_rsupl:
                      th_rsuplsb:
                      th_tsupl:
                      th_tsuplsb:
                      th_prod:
                      th_birth:
                      th_lastupd:
                      th_copied);
       endsr;
      /end-free
      * ***************************************************** End of program

