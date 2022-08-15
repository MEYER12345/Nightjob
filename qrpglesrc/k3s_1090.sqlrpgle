      *****************************************************************
     h copyright('(C) Copyright 1996 - 2003 King III Solutions, Inc.  +
     h Rel 4.31 2003-01-03       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2003 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1090
      **   Type: ILE RPG Program
      **   Desc: Alternate source - Pro Forma Report
      **
      *****************************************************************
      **
      **  This program is used to print the alternate source Pro Forma
      **  report, that is faxed out to alternate source suppliers
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/03/2014.
      **  Remarks. Altered program to utilize SQL select statements to
      **           access K_ COMPANY, K_SUPLSOQ, K_BUYRGRP, K_LOCATNS,
      **           and K_PRODUCT records instead of using RPG CHAIN
      **           operation codes. Also, utilized an SQL cursor to
      **           loop through K_PRODSOQ records and write detail
      **           lines for report.
      *****************************************************************


     fk3s_p1090 o    e             printer oflind(over_flow)
     f
      * printer file

      * ----------------------------------------------------- get time stamp
     d lda_usrdat      s              4                                         user date format
     d lda_usrtim      s              4                                         user time format
     d lda_usradj      s              3p 0                                      user time adj. hours

      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz                                  time stamp
     d time            s               t                                        time
     d date            s               d                                        date
     d x               s              3p 0                                      # valid locations
     d y               s              3p 0                                      step through locns
     d first_page      s              1    inz(*off)                            first page printed
     d pr_ndc_upc      s             25                                         first page printed

      * --------------------------------------------------- Named indicators
     d over_flow       s               n                                        print overflow

      * -------------------------------------- Program Status Data Structure
     d*copy k3s_c040
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C040
      **   Type: ILE /COPY member
      **   Desc: *PROC Program Status Data Structure fields
      **
      *****************************************************************

     d                sds
     d psds_progm        *proc                                                  program name
     d psds_error             90    170                                         error
     d psds_user             254    263                                         user ID

      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_1090        PI
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5p 0
     d  ponumber                     10
     d  pomessage1                   25
     d  pomessage2                   25
     d  pomessage3                   25
     d  pomessage4                   25
     d  pomessage5                   25
     d  pomessage6                   25
     d  pomessage7                   25
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d company_rec   e ds                  ExtName(k_company)
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d buyrgrp_rec   e ds                  ExtName(k_buyrgrp)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
     d*
      /free
       //--------------------------------------------------------- main line
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //get header information
       exec sql
         select *
           into :suplsoq_rec
           from k_suplsoq
           where so_comp = :comp and
                 so_buyr = :buyr and
                 so_locn = :locn and
                 so_supl = :supl and
                 so_suplsub = :suplsub and
                 so_soqseq# = :soqseq#
           fetch first row only;
       if SQLState = SQLStateOk;
          supl_desc = %trimr(supl) + ' ' + so_name;
       endif;

       //get buy group record
       exec sql
         select *
           into :buyrgrp_rec
           from k_buyrgrp
           where by_comp = :comp and
                 by_buyr = :buyr
           fetch first row only;
       if SQLState = SQLStateOk;
          buy_group = %trimr(buyr) + ' ' + by_name;
       endif;

       //get location description
       exec sql
         select *
           into :locatns_rec
           from k_locatns
           where lc_comp = :comp and
                 lc_locn = :locn
           fetch first row only;
       if SQLState = SQLStateOk;
          locn_desc = %trimr(locn) + ' ' + lc_desc;
       endif;

       exsr dclpqcursor;
       exsr opnpqcursor;
       //---------------------------------------------------- start products
       //start with first product for this supplier suggested order

       Dow SQLState = SQLStateOk;
       //read products in order
          exec sql
            fetch next
              from pqcursor
              into :prodsoq_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

       //only process when record read and soq exists
          if pq_soqact > 0;

             tot_record += 1;

       //get ndc_upc id
             exec sql
             select pr_ndc_upc
                into :pr_ndc_upc
                from k_product
                where pr_comp = :comp and
                      pr_locn = :locn and
                      pr_supl = :supl and
                      pr_suplsub = :suplsub and
                      pr_prod = :pq_prod
                fetch first row only;
             if SQLState = RowNotFound;
               SQLState = SQLStateOk;
             endif;
       //print product
             exsr     $_print;

          endif;
       enddo;

       exsr clspqcursor;
       //------------------------------------------------------ end products

       *inlr = *on;

       //print total line
       if *inlr = *on;
          write     k3s_r03;
       endif;
       /////////////////////////////////////////////////////////////// Print

       begsr $_print;

       // print header on first page or overflow
       if first_page = *off  or
          over_flow = *on;
             first_page = *on;
             over_flow = *off;

             write k3s_r01;
       endif;                                                              //header page

       // calculate dollar extension
       extension = pq_soqact *
                   pq_costord/pq_costdiv;
                   pq_desc1x = pq_desc1;
       // print detail line
       write     k3s_r02;

       // accumulate total dollars
       total_$ += extension;

       endsr;

       //////////////////////////////////////////////////////////// One time

       begsr *inzsr;

       //get company code
       exec sql
          select *
            into :company_rec
            from k_company
            where cm_comp = :comp
            fetch first row only;

       //prime program id
       zz_program = psds_progm;

       //prime company code
       zz_compcod = cm_compcod;

       //prime user id
       zz_user = psds_user;

       //report title
       zz_title  = 'Product Request Report  ';
       zz_title2 = cm_cmpname;

       //----------------------------------------------------- get timestamp
       //call module to retrieve timestamp

       callp K3S_Retrieve_Timestamp(time_stamp);

       //------------------------------------------------- get time formated
       lda_usrdat = cm_reptdat;
       lda_usrtim = cm_repttim;
       lda_usradj = 0;
       //get tiem formatted
       exsr      $_get_time;

       endsr;

       //----------------------------------------get time subroutine
       //copy k3s_c180
       //**************************************************************
       //
       //  K3S-Replenish - Inventory REPLENISHment System
       //  Copyright (c) 1996-1997 by King III Solutions, Inc.
       //  All rights reserved.
       //
       //**************************************************************
       //
       //  Name: K3S_C180
       //  Type: ILE /COPY member
       //  Desc: Get system date and time formated       'C' specs only
       //
       //**************************************************************

       ///////////////////////////////////////// Get date and time formated

       begsr $_get_time;

       //-------------------------------------------- Retrieve date and time
       //call module to retrieve user formated date and time
       callp K3S_Retrieve_Date_Time(lda_usrdat:
                                    lda_usrtim:
                                    lda_usradj:
                                    zz_usrdate:
                                    zz_usrtime);
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
         for
         select *
         from k_prodsoq
         where pq_comp = :comp and
               pq_buyr = :buyr and
               pq_locn = :locn and
               pq_supl = :supl and
               pq_suplsub = :suplsub and
               pq_soqseq# = :soqseq#
         order by pq_comp,
                  pq_buyr,
                  pq_locn,
                  pq_supl,
                  pq_suplsub,
                  pq_soqseq#,
                  pq_suplorn,
                  pq_suplorg,
                  pq_suplors,
                  pq_prodseq;
       endsr;

       begsr opnpqcursor;
       exec sql
        open pqcursor;
        if SQLState <> SQLStateOk;
           exsr clspqcursor;
           exec sql
            open pqcursor;
        endif;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;
      /end-free

