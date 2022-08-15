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
      **   Name: K3S_3042
      **   Type: ILE RPG Program
      **   Desc: Supplier switch for product is allowed and has occured
      **
      *****************************************************************
      **
      **  This program will change the supplier in up to 6 files:
      **      K_PRODFOR - Forecast changes log
      **      K_NOTEPAD - Product notes
      **      K_PRODHLD - Product hold out quantities
      **      K_PRODLOG - Product changes log
      **      K_PHLDLOG - Product hold out changes log
      **      K_DLYPROD - Daily products information capture
      **
      **  A previously run OPNQRYF will supply only the code '2'
      **  k_logprod records to be processed for today's date.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 10/06/2014.
      *   Remarks. Commented out OPNQRYF statements in K3S_NITECL
      *            pertaining to K3S_3042 leaving call to K3S_3042
      *            and parameters to pass. Changed K3S_3420 declaring
      *            an SQL cursor to select K_LOGPROD records and loop
      *            through them based on parameters passed. Added
      *            dynamic SQL cursors to loop through files K_PRODFOR,
      *            K_NOTEPAD, K_PRODHLD, K_PRODLOG, K_PHLDLOG, and
      *            K_DLYPROD and added routimes to update each with
      *            new supplier.
      *****************************************************************
      * ---------------------------------------------------------- Begin
     d logprod_rec   e ds                  ExtName(k_logprod)
     d prodfor_rec   e ds                  ExtName(k_prodfor)
     d notepad_rec   e ds                  ExtName(k_notepad)
     d prodhld_rec   e ds                  ExtName(k_prodhld)
     d prodlog_rec   e ds                  ExtName(k_prodlog)
     d phldlog_rec   e ds                  ExtName(k_phldlog)
     d dlyprod_rec   e ds                  ExtName(k_dlyprod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3042        PI
     d  comp                          1
     d  today_str                    26
     d  today_end                    26
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d chk_timestp     s             26
     d chk_logtype     s              1
     d chk_comp        s              1
     d #file           s              9a   inz
     d #locn           s                   like(pf_locn)
     d #supl           s                   like(pf_supl)
     d #suplsub        s                   like(pf_suplsub)
     d #prod           s                   like(pf_prod)
     d #typval         s                   like(nt_typval)
     d begin_date      s               d   inz(d'0001-01-01')
     d #lglocn         s                   like(lg_locn)
     d #lgbuyr         s                   like(lg_buyr)
     d #lgsupl         s                   like(lg_supl)
     d #lgsuplsub      s                   like(lg_suplsub)
     d #lgsuplusr      s                   like(lg_suplusr)
     d #lgsuplusb      s                   like(lg_suplusb)
     d #lgprod         s                   like(lg_prod)
      * ---------------------------------------------------------- Begin
      /free
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclpfcursor;
       exsr dclntcursor;
       exsr dclpucursor;
       exsr dclp0cursor;
       exsr dclh0cursor;
       exsr dclr1cursor;

       //declare and open lgcursor for k_logprod
       //fetch first product log record
       exsr dcllgcursor;
       exsr clslgcursor;
       exsr opnlgcursor;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
       dow SQLState = SQLStateOk;

         exec sql
          fetch next
          from lgcursor
          into :logprod_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          chk_comp    = lg_comp;
          chk_timestp = %char(lg_timestp);
          chk_logtype = lg_logtype;

          #lglocn = lg_locn;
          #lgbuyr = lg_buyr;
          #lgsupl = lg_supl;
          #lgsuplsub = lg_suplsub;
          #lgsuplusr = lg_suplusr;
          #lgsuplusb = lg_suplusb;
          #lgprod = lg_prod;
       //-------------------------------------- change supplier in k_prodfor
          #file = 'K_Prodfor';
       //  set to previous supplier
          exsr InzInpSrchpf;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmtpf;
          if SQLState = SQLStateOk;
            exsr opnpfcursor;
       //start with first occurence of this product at previous supplier
        //do until end of file, OR until we change products
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from pfcursor
                into :prodfor_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

              #locn = pf_locn;
              #supl = pf_supl;
              #suplsub = pf_suplsub;
              #prod = pf_prod;

        //Update with new supplier.
              pf_supl = lg_supl;
              pf_suplsub = lg_suplsub;
              exsr updtprodfor;
            enddo;
            exsr clspfcursor;
          endif;
        //------------------------------------- change supplier in k_notepad

        // set to previous supplier
        //Start with first occurance of this product at previous supplier
          #file = 'K_Notepad';
          #typval = 'P';
          exsr InzInpSrchnt;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmtnt;
          if SQLState = SQLStateOk;
            exsr opnntcursor;
       //start with first occurence of this product at previous supplier
        //do until end of file, OR until we change products
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from ntcursor
                into :notepad_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

        //Update with new supplier.
              nt_supl = lg_supl;
              nt_suplsub = lg_suplsub;
              exsr updtnotepad;
            enddo;
            exsr clsntcursor;
          endif;

         //------------------------------------ change supplier in k_prodhld

        // set to previous supplier

          #file = 'K_Prodhld';
       //  set to previous supplier
          exsr InzInpSrchpu;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmtpu;
          if SQLState = SQLStateOk;
            exsr opnpucursor;

        //start with first occurance of this product at previous supplier
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from pucursor
                into :prodhld_rec;

              if SQLState = RowNotFound;
                leave;
              endif;
        //Update with new supplier.
              pu_supl = lg_supl;
              pu_suplsub = lg_suplsub;
              exsr updtprodhld;
            enddo;
            exsr clspucursor;
          endif;

        //------------------------------------- change supplier in k_prodlog

        // set to previous supplier
          #file = 'K_Prodlog';
       //  set to previous supplier
          exsr InzInpSrchp0;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmtp0;
          if SQLState = SQLStateOk;
            exsr opnp0cursor;
       //start with first occurence of this product at previous supplier
        //do until end of file, OR until we change products
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from p0cursor
                into :prodlog_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

        //Update with new supplier.
              p0_supl = lg_supl;
              p0_suplsub = lg_suplsub;
              exsr updtprodlog;
            enddo;
            exsr clsp0cursor;
          endif;

         //------------------------------------ change supplier in k_phldlog

         //set to previous supplier

          #file = 'K_Phldlog';
       //  set to previous supplier
          exsr InzInpSrchh0;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmth0;
          if SQLState = SQLStateOk;
            exsr opnh0cursor;

        //start with first occurance of this product at previous supplier
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from h0cursor
                into :phldlog_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

        //Update with new supplier.
              h0_supl = lg_supl;
              h0_suplsub = lg_suplsub;
              exsr updtphldlog;
            enddo;
            exsr clsh0cursor;
          endif;

        //------------------------------------- change supplier in k_dlyprod

          #file = 'K_Dlyprod';
       //  set to previous supplier
          exsr InzInpSrchr1;
          exsr IntSQLStmt;
          exsr PrepDynSQLStmtr1;
          if SQLState = SQLStateOk;
            exsr opnr1cursor;

        //start with first occurance of this product at previous supplier
            dow SQLState = SQLStateOk;

              exec sql
               fetch next
                from r1cursor
                into :dlyprod_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

        //Update with new supplier.
              r1_suplusr = lg_suplusr;
              r1_suplusb = lg_suplusb;
              exsr updtdlyprod;
            enddo;
            exsr clsr1cursor;
          endif;

        //------------------------------------- end of changes this _logprod

        //read products end of loop
       enddo;

       exsr clslgcursor;
        //------------------------------------------------- End of Main Loop

        //finished, set on LR
       *inlr = *on;

       begsr dcllgcursor;
       exec sql
        declare lgcursor Cursor
          for
        select *
          from k_logprod
          where lg_comp = :comp and
                lg_logtype = '2' and   ------- '2' = supplier changed
                char(lg_timestp) >= :today_str and
                char(lg_timestp) <= :today_end;
       endsr;

       begsr opnlgcursor;
       exec sql
        open lgcursor
        if SQLState <> SQLStateOk;
           exsr clslgcursor;
           exec sql
            open lgcursor;
        endif;
       endsr;

       begsr clslgcursor;
       exec sql
        close lgcursor;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From ' + #file +
                   ' ' + 'Where';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchpf;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pf_comp = ? and +
                     pf_locn = ? and +
                     pf_supl = ? and +
                     pf_suplsub = ? and +
                     pf_prod = ? +
                     Order by pf_comp, +
                              pf_locn, +
                              pf_supl, +
                              pf_suplsub, +
                              pf_prod, +
                              pf_birth desc, +
                              pf_birthtm desc';
       endsr;

       begsr dclpfcursor;
       exec sql
        declare pfcursor Cursor
         for DynSQLStmtpf;
       endsr;

       begsr PrepDynSQLStmtpf;
       exec sql
        Prepare DynSqlStmtpf
          From :StmtString;
       endsr;

       begsr opnpfcursor;
       exec sql
        open pfcursor
          using :lg_comp,
                :lg_locn,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod;
       endsr;

       begsr updtprodfor;
        exec sql
          update k_prodfor
            set pf_supl = :pf_supl,
                pf_suplsub = :pf_suplsub
            where current of pfcursor;
       endsr;

       begsr clspfcursor;
       exec sql
        close pfcursor;
       endsr;

       Begsr InzInpSrchnt;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'nt_comp = ? and +
                     nt_locn = ? and +
                     nt_typval = ? and +
                     nt_supl = ? and +
                     nt_suplsub = ? and +
                     nt_prod = ? +
                     Order by nt_comp, +
                              nt_locn, +
                              nt_typval, +
                              nt_supl, +
                              nt_suplsub, +
                              nt_prod';
       endsr;

       begsr dclntcursor;
       exec sql
        declare ntcursor Cursor
         for DynSQLStmtnt;
       endsr;

       begsr PrepDynSQLStmtnt;
       exec sql
        Prepare DynSqlStmtnt
          From :StmtString;
       endsr;

       begsr opnntcursor;
       exec sql
        open ntcursor
          using :lg_comp,
                :lg_locn,
                :#typval,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod;
       endsr;

       begsr updtnotepad;
        exec sql
          update k_notepad
            set nt_supl = :nt_supl,
                nt_suplsub = :nt_suplsub
            where current of ntcursor;
       endsr;

       begsr clsntcursor;
       exec sql
        close ntcursor;
       endsr;

       Begsr InzInpSrchpu;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pu_comp = ? and +
                     pu_locn = ? and +
                     pu_supl = ? and +
                     pu_suplsub = ? and +
                     pu_prod = ? and +
                     pu_begin >= ? +
                     Order by pu_comp, +
                              pu_locn, +
                              pu_supl, +
                              pu_suplsub, +
                              pu_prod, +
                              pu_begin';
       endsr;

       begsr dclpucursor;
       exec sql
        declare pucursor Cursor
         for DynSQLStmtpu;
       endsr;

       begsr PrepDynSQLStmtpu;
       exec sql
        Prepare DynSqlStmtpu
          From :StmtString;
       endsr;

       begsr opnpucursor;
       exec sql
        open pucursor
          using :lg_comp,
                :lg_locn,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod,
                :begin_date;
       endsr;

       begsr updtprodhld;
        exec sql
          update k_prodhld
            set pu_supl = :pu_supl,
                pu_suplsub = :pu_suplsub
            where current of pucursor;
       endsr;

       begsr clspucursor;
       exec sql
        close pucursor;
       endsr;

       Begsr InzInpSrchp0;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'p0_comp = ? and +
                     p0_locn = ? and +
                     p0_supl = ? and +
                     p0_suplsub = ? and +
                     p0_prod = ? +
                     Order by p0_comp, +
                              p0_locn, +
                              p0_supl, +
                              p0_suplsub, +
                              p0_prod, +
                              p0_birth desc, +
                              p0_birthtm desc';
       endsr;

       begsr dclp0cursor;
       exec sql
        declare p0cursor Cursor
         for DynSQLStmtp0;
       endsr;

       begsr PrepDynSQLStmtp0;
       exec sql
        Prepare DynSqlStmtp0
          From :StmtString;
       endsr;

       begsr opnp0cursor;
       exec sql
        open p0cursor
          using :lg_comp,
                :lg_locn,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod;
       endsr;

       begsr updtprodlog;
        exec sql
          update k_prodlog
            set p0_supl = :p0_supl,
                p0_suplsub = :p0_suplsub
            where current of p0cursor;
       endsr;

       begsr clsp0cursor;
       exec sql
        close p0cursor;
       endsr;

       Begsr InzInpSrchh0;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'h0_comp = ? and +
                     h0_locn = ? and +
                     h0_supl = ? and +
                     h0_suplsub = ? and +
                     h0_prod = ? +
                     Order by h0_comp, +
                              h0_locn, +
                              h0_supl, +
                              h0_suplsub, +
                              h0_prod, +
                              h0_birth desc, +
                              h0_birthtm desc';
       endsr;

       begsr dclh0cursor;
       exec sql
        declare h0cursor Cursor
         for DynSQLStmth0;
       endsr;

       begsr PrepDynSQLStmth0;
       exec sql
        Prepare DynSqlStmth0
          From :StmtString;
       endsr;

       begsr opnh0cursor;
       exec sql
        open h0cursor
          using :lg_comp,
                :lg_locn,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod;
       endsr;

       begsr updtphldlog;
        exec sql
          update k_phldlog
            set h0_supl = :h0_supl,
                h0_suplsub = :h0_suplsub
            where current of h0cursor;
       endsr;

       begsr clsh0cursor;
       exec sql
        close h0cursor;
       endsr;

       Begsr InzInpSrchr1;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'r1_comp = ? and +
                     r1_locn = ? and +
                     r1_suplusr = ? and +
                     r1_suplusb = ? and +
                     r1_prod = ? +
                     Order by r1_comp, +
                              r1_locn, +
                              r1_suplusr, +
                              r1_suplusb, +
                              r1_prod, +
                              r1_birth desc, +
                              r1_batch#';
       endsr;

       begsr dclr1cursor;
       exec sql
        declare r1cursor Cursor
         for DynSQLStmtr1;
       endsr;

       begsr PrepDynSQLStmtr1;
       exec sql
        Prepare DynSqlStmtr1
          From :StmtString;
       endsr;

       begsr opnr1cursor;
       exec sql
        open r1cursor
          using :lg_comp,
                :lg_locn,
                :lg_prvsupl,
                :lg_prvsub,
                :lg_prod;
       endsr;

       begsr updtdlyprod;
        exec sql
          update k_dlyprod
            set r1_suplusr = :r1_suplusr,
                r1_suplusb = :r1_suplusb
            where current of r1cursor;
       endsr;

       begsr clsr1cursor;
       exec sql
        close r1cursor;
       endsr;
      /end-free
