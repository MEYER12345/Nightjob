      *****************************************************************
     h copyright('(C) Copyright 1996 - 2018 King III Solutions, Inc.  +
     h Rel 5.5  2018-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2018 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3043
      **   Type: ILE RPG Program
      **   Desc: Adjust K_INTDALY for supplier switch
      **
      *****************************************************************
      **
      **  When K_LOGPROD has supplier switch LG_LOGTYPE = '2' records,
      **  adjust both IE_SUPL
      **              IE_SUPLSUB
      **
      *****************************************************************
      **
      **  This program can run on its own to patch records in the past,
      **  or it can process just the new LG_LOGTYPE = '2' records
      **  from the K3S night job.
      **
      **  A parm of &TODAY_STR is developed in K3S_9010 from the
      **  K3S night job, and can be used in exit point K3S_X310CL
      **  to tell this program to read just today's records.
      **
      **  or, just pass a blank for parm &TODAY_STR, and this
      **  program will read the all LG_LOGTYPE = '2' records
      **  in the file. A location/product might have multiple
      **  LG_LOGTYPE = '2' records, and that is no problem
      **  for this program.
      **
      *****************************************************************

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)

     d logprod_rec   e ds                  ExtName(k_logprod)
     d intprod_rec   e ds                  ExtName(k_intprod)
     d intdaly_rec   e ds                  ExtName(k_intdaly)
      * --------------------------------------------------
     d time_stamp      s               z
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed
     d K3S_3043        PI
     d  comp                          1
     d  timestp                      26
      *--------------------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //-----  look for supplier switch records only, and don't include *SUM
       if timestp = *blanks or
          timestp = '0001-01-01-00.00.00.000000';
            clear time_stamp;
       else;
            time_stamp = %timestamp(timestp);
       endif;

       exsr dcliecursor;
       exsr dcllgcursor;
       exsr opnlgcursor;

       dow SQLState = SQLStateOk;

          exec sql
             fetch next
                from lgcursor
                   into :logprod_rec;

          if SQLState = RowNotFound;
             leave;
          endif;

          if lg_logtype = '2' and
             lg_locn <> '*SUM';

       //---------- must be a current product in database
       //---------- and you want the current SUPLUSR, SUPLUSB values

             exec sql
                select *
                   into :intprod_rec
                   from k_intprod
                   where ip_comp = :lg_comp and
                         ip_locn = :lg_locn and
                         ip_prod = :lg_prod
                   fetch first row only;

             if SQLState = SQLStateOk;

                exsr InzInpSrch;
    ‚   //Initialize StmtString
  ‚             exsr intSQLStmt;
    ‚   //Prepare dynamic SQL statement
  ‚             exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;   //if prep was good
    ‚   //Open dynamic curosr
                   exsr opniecursor;

                   Dow SQLState = SQLStateOk;
                       exec sql
                        fetch next
                          from iecursor
                          into :intdaly_rec;

                       if SQLState = RowNotFound;
                          leave;
                       endif;

       //  process K_INTDALY records - begin
                      if ie_supl  <> ip_suplusr or
                         ie_suplsub <> ip_suplusb;

                         exsr $_update_intdaly;
                      endif;

                   enddo;
                endif;
      *--  process K_INTDALY records - end
             endif;
          endif;
      *------------ must be a current product in database
       enddo;

       exsr clslgcursor;

       *inlr = *on;

       begsr dcllgcursor;
       exec sql
       Declare lgcursor cursor
         for
         select *
         from k_logprod
         where lg_comp = :comp and
               lg_timestp >= :time_stamp
         order by lg_comp,
                  lg_timestp;
       endsr;

       begsr opnlgcursor;
         exec sql
           open lgcursor;
       endsr;

       begsr clslgcursor;
         exec sql
           close lgcursor;
       endsr;

       begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Intdaly +
                   Where ';
       //
       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr InzInpSrch;

       InpSrchcnd = *blanks;
       InpSrchCnd = 'ie_comp = ? and +
                     ie_locn = ? and +
                     ie_prod = ? and +
                     Order by ie_comp, +
                              ie_locn, +
                              ie_prod, +
                              ie_birth desc, +
                              ie_supl, +
                              ie_suplsub';
       endsr;

       begsr opniecursor;
       exec sql
        open iecursor
         using :ip_comp,
               :ip_locn,
               :ip_prod;
       endsr;

       begsr clsiecursor;
       exec sql
        close iecursor;
       endsr;

       begsr dcliecursor;
       exec sql
        declare iecursor Cursor
         for DynSQLStmt;
       endsr;

       begsr $_update_intdaly;
       exec sql
          update k_intdaly
            set ie_supl    =  :ip_suplusr,
                ie_suplsub =  :ip_suplusb
            where current of iecursor;
       endsr;
      /free
