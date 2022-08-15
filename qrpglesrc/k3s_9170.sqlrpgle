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
      **   Name: K3S_9170
      **   Type: ILE RPG Program
      **   Desc: Change current year and period for alt source records
      **
      *****************************************************************
      **
      **  This program will be used to keep the current year and
      **  period up to date for alternate source records, when a
      **  period end process has taken place, and alternate source
      **  records exist that are being copied to a holding spot
      **  during the week.
      **
      **  Two previously run OPNQRYF's will supply only the alternate
      **  source records for the k_product and k_prodsoq files,
      **  identified by pr_altsrce = 1 and pq_altsrce = 1 respectively.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 05/11/2014.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_9170
      *            from K3S_NITECL leaving the call to RPG program
      *            K3S_9170 and passing the company code and altsrce as
      *            parameters. Used SQL Select statements to declare
      *            cursors for k_product and k_prodsoq and to only
      *            select rows where the company column matched the
      *            passed column parameter and where the altsrce field
      *            matched the passed altsrce parameter. In K3S_9170,
      *            changed to use embedded SQL utilizing a cursor
      *            to loop through k_product and k_prodsoq records
      *            and also to update the k_product and k_prodsoq
      *            records. Also, changed to access k_locatns through
      *            an SQL Select statement.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d locn            s                   like(pr_locn)
      * --------------------------------------------------- parameter passed prototype
     d product_rec   e ds                  ExtName(k_product)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
     d locatns_rec   e ds                  ExtName(k_locatns)

     d/copy k3s_proto
      * --------------------------------------------------  procedure interface
     d K3S_9170        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      /free
       //---------------------------------------- start k_product processing
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclprcursor;
       exsr clsprcursor;
       exsr opnprcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOK;

       //read next product
          exec sql
           fetch next
            from prcursor
            into :product_rec;

          if SQLState = RowNotFound;
             Leave;
          endif;

       //---------------------------------------------------- Location break
       //change in location
          if pr_locn <> locn;
             locn = pr_locn;

       //get location
             exec sql
              Select *
              Into :locatns_rec
              From k_locatns
              Where lc_comp = :pr_comp and
                    lc_locn = :pr_locn
              Fetch first row only;
          endif;
       //--------------------------------------------- update k_product file

           select;

       //     weekly forecasting
              when pr_forcint = 52;
                   pr_forcper = lc_perd52;
                   pr_forcyr  = lc_year52;

       //     monthly forecasting
              when pr_forcint = 12;
                   pr_forcper = lc_perd12;
                   pr_forcyr  = lc_year12;

       //     thirteen four-weekly forecasting
              when pr_forcint = 13;
                   pr_forcper = lc_perd13;
                   pr_forcyr  = lc_year13;

           endsl;

           exsr updtproduct;

       enddo;

       exsr clsprcursor;
       //---------------------------------------- start k_prodsoq processing
       exsr dclpqcursor;
       exsr clspqcursor;
       exsr opnpqcursor;

       locn = *blanks;

       //main loop
       dow SQLState = SQLStateOK;

       //read next prodsoq
          exec sql
           fetch next
            from pqcursor
            into :prodsoq_rec;

          if SQLState = RowNotFound;
             Leave;
          endif;
       //---------------------------------------------------- Location break
       //change in location
          if pq_locn <> locn;
             locn    = pq_locn;

       //get location
              exec sql
               Select *
                Into :locatns_rec
                From k_locatns
                Where lc_comp = :pq_comp and
                      lc_locn = :pq_locn
                Fetch first row only;

          endif;

       //--------------------------------------------- update k_prodsoq file

           select;

       //     weekly forecasting
              when pq_forcint = 52;
                   pq_forcper = lc_perd52;

       //     monthly forecasting
              when pq_forcint = 12;
                   pq_forcper = lc_perd12;

       //     thirteen four-weekly forecasting
              when pq_forcint = 13;
                   pq_forcper = lc_perd13;

           endsl;

           exsr updtprodsoq;

         enddo;

       exsr clspqcursor;
       //------------------------------------------------------- End program

       //finished, set on LR
       *inlr = *on;

       begsr dclprcursor;
       exec sql
        Declare prcursor Cursor
         for
         Select *
         From k_product
         Where pr_comp = :comp and
               pr_altsrce = 1
         Order by pr_locn
         For update of pr_forcper,
                       pr_forcyr;
       endsr;

       begsr dclpqcursor;
       exec sql
        Declare pqcursor Cursor
         for
         Select *
         From k_prodsoq
         Where pq_comp = :comp and
               pq_altsrce = 1
         Order by pq_locn
         For update of pq_forcper;
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor;
        if SQLState <> SQLStateOk;
           exsr clsprcursor;
           exec sql
            open prcursor;
        endif;
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

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

       begsr updtproduct;
       exec sql
         update k_product
           set pr_forcper = :pr_forcper,
               pr_forcyr  = :pr_forcyr
           where current of prcursor;
       endsr;

       begsr updtprodsoq;
       exec sql
         update k_prodsoq
           set pq_forcper = :pq_forcper
           where current of pqcursor;
       endsr;
      /end-free
