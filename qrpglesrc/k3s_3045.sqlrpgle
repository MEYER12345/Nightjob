      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 4.41 2008-01-01       Program Property of King III Solutions, Inc. +
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
      **   Name: K3S_3045
      **   Type: ILE RPG Program
      **   Desc: Remove product history records after product deleted
      **
      *****************************************************************
      **
      **  This program will remove all product history records once
      **  the product has been deleted from K_PRODUCT. This program
      **  runs in the K3S_NITECL process, and for that batch process
      **  where file K_LOGPROD contains LG_LOGTYPE = '4' records,
      **  then look in both K_PRODHIS and K_PRODH52 files and delete
      **  all occurances that match. This includes not only the
      **  PH_HISTYPE = 0 and PW_HISTYPE = 0 records, but all additional
      **  history types. This program only runs when the NJP Night Job
      **  Parameter has been set on for RMV_HISTRY.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/20/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_LOGPROD. Also, changed it to
      *            use SQL cursors to loop through the desired
      *            records in K_PRODHIS and K_PRODH52 and utilized
      *            an SQL delete statement to remove them.
      *****************************************************************
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d logprod_rec   e ds                  ExtName(k_logprod)
     d prodhis_rec   e ds                  ExtName(k_prodhis)
     d prodh52_rec   e ds                  ExtName(k_prodh52)
     d                                     Prefix(p5:2)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3045        PI
     d  comp                          1
     d  log_type                      1
     d  today_str                    26
     d  end_1500                     26

      /free

        //-------------------------------------------------------- Begin
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclphcursor;
       //exsr clsphcursor;
       exsr dclpwcursor;
       //exsr clspwcursor;

       exsr dcllgcursor;
       //exsr clslgcursor;
       exsr opnlgcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOK;

       //fetch product log record
          exec sql
           fetch next
            from lgcursor
            into :logprod_rec;

          if SQLState = RowNotFound;
             leave;
          endif;

       //-------------------------------------- remove records in k_prodhis
          exsr InzInpSrch1;
    ‚   //initialize StmtString
    ‚      exsr intSQLStmt1;
    ‚   //prepare statement
    ‚      exsr prepDynSQLStmt;
          if SQLState = SQLStateOk; //If prep is successful
    ‚   //open dynamic cursor
             exsr opnphcursor;
       //start with first occurance of this product
       //do until end of file, OR until we change products
             dow SQLState = SQLStateOk;

               exec sql
                fetch next
                 from phcursor
                 into :prodhis_rec;

               if SQLState = RowNotFound;
                 leave;
               endif;

               if SQLState = SQLStateOk;

                 exec sql
                  Delete from k_prodhis
                    Where current of phcursor;
               endif;

             enddo;

             exsr clsphcursor;

          endif;
        //------------------------------------- remove records in k_prodh52

        //set to supplier

          exsr InzInpSrch2;
    ‚   //initialize StmtString
    ‚      exsr intSQLStmt2;
    ‚   //prepare statement
    ‚      exsr prepDynSQLStmt2;
          if SQLState = SQLStateOk; //If prep is successful
    ‚   //open dynamic cursor
             exsr opnpwcursor;
       //set to supplier
       //start with first occurance of this product
       //do until end of file, OR until we change products

             dow SQLState = SQLStateOk;
                exec sql
                  fetch next
                   from pwcursor
                   into :prodh52_rec;

                if SQLState = RowNotFound;
                  leave;
                endif;

                if SQLState = SQLStateOK;
                  exec sql
                    Delete from k_prodh52
                    Where current of pwcursor;
                endif;
             enddo;

             exsr clspwcursor;
          endif;
       //------------------------------------ end of deletes this _logprod
         //read products end of loop
          SQLState = SQLStateOk;
       enddo;

       exsr clslgcursor;
       //------------------------------------------------- End of Main Loop

       //finished, set on LR
        *inlr = *on;

       //log product transactions - today's records only, for type '4'
       begsr dcllgcursor;
       exec sql
        Declare lgcursor Cursor
         for
         Select *
         From k_logprod
         Where lg_comp = :comp and
               lg_logtype = :log_type and
               char(lg_timestp) >= :today_str and
               char(lg_timestp) <= :end_1500;
       endsr;

       begsr opnlgcursor;
       exec sql
        open lgcursor;
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

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr PrepDynSQLStmt2;
       exec sql
        Prepare DynSqlStmt2
          From :StmtString;
       endsr;

       Begsr IntSQLStmt1;
       String = *blanks;
       String =   'Select * +
                   From K_Prodhis +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr IntSQLStmt2;
       String = *blanks;
       String =   'Select * +
                   From K_Prodh52 +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch1;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'ph_comp = ? and +
                     ph_locn = ? and +
                     ph_supl = ? and +
                     ph_suplsub = ? and +
                     ph_prod = ? +
                     Order by ph_comp, +
                              ph_locn, +
                              ph_supl, +
                              ph_suplsub, +
                              ph_prod';
       endsr;

       Begsr InzInpSrch2;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pw_comp = ? and +
                     pw_locn = ? and +
                     pw_supl = ? and +
                     pw_suplsub = ? and +
                     pw_prod = ? +
                     Order by pw_comp, +
                              pw_locn, +
                              pw_supl, +
                              pw_suplsub, +
                              pw_prod';
       endsr;

       begsr opnphcursor;
       exec sql
        open phcursor
          using :lg_comp,
                :lg_locn,
                :lg_supl,
                :lg_suplsub,
                :lg_prod;
        endsr;

       begsr opnpwcursor;
       exec sql
        open pwcursor
          using :lg_comp,
                :lg_locn,
                :lg_supl,
                :lg_suplsub,
                :lg_prod;
        endsr;

       begsr clsphcursor;
       exec sql
        close phcursor;
       endsr;

       begsr dclphcursor;
       exec sql
        declare phcursor Cursor
         for DynSQLStmt;
        endsr;

       begsr dclpwcursor;
       exec sql
        declare pwcursor Cursor
         for DynSQLStmt2;
       endsr;

       begsr clspwcursor;
       exec sql
        close pwcursor;
       endsr;
      /end-free

