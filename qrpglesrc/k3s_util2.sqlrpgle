      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h OPTION(*NODEBUGIO)

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
      **   Name: K3S_UTIL2
      **   Type: ILE RPG Program
      **   Desc: Change location from current to new, both passed to
      **         the program as parameters.
      *****************************************************************
      **
      *****************************************************************
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d suplier_rec   e ds                  ExtName(k_suplier)
     d product_rec   e ds                  ExtName(k_product)
     d prodhis_rec   e ds                  ExtName(k_prodhis)
      * ------------------------------------------------------parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_UTIL2       PI
     d  curlocn                       5a
     d  newlocn                       5a
      * ---------------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                  datfmt = *iso;
       //
       exsr process_suplier;
       exsr process_product;
       exsr process_prodhis;

       *inlr = '1';
       //
       begsr process_suplier;

       exsr dclspcursor;
       exsr clsspcursor;
       exsr opnspcursor;
       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //fetch supplier rows from result set
         exec sql
          fetch next
            from spcursor
            into :suplier_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

         exsr updtsupl;

       enddo;
       exsr clsspcursor;

       endsr;

       begsr process_product;

       exsr dclprcursor;
       exsr clsprcursor;
       exsr opnprcursor;
       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //fetch product rows from result set
         exec sql
          fetch next
            from prcursor
            into :product_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

         exsr updtprod;

       enddo;
       exsr clsprcursor;

       endsr;

       begsr process_prodhis;

       exsr dclphcursor;
       exsr clsphcursor;
       exsr opnphcursor;
       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //fetch supplier rows from result set
         exec sql
          fetch next
            from phcursor
            into :prodhis_rec;

         if SQLState = RowNotFound;
            Leave;
         endif;

         exsr updthist;

       enddo;
       exsr clsphcursor;

       endsr;

       begsr dclspcursor;
       exec sql
        Declare spcursor Cursor
         for
         Select *
         from k_suplier
         where sp_locn = :curlocn;
       endsr;

       begsr opnspcursor;
       exec sql
        open spcursor;
       endsr;

       begsr clsspcursor;
       exec sql
        close spcursor;
       endsr;

       begsr updtsupl;
       exec sql
        update k_suplier
         set sp_locn = :newlocn
         where current of spcursor;
       endsr;

       begsr dclprcursor;
       exec sql
        Declare prcursor Cursor
         for
         Select *
         from k_product
         where pr_locn = :curlocn;
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr updtprod;
       exec sql
        update k_product
          set pr_locn = :newlocn
          where current of prcursor;
       endsr;

       begsr dclphcursor;
       exec sql
        Declare phcursor Cursor
         for
         Select *
         from k_prodhis
         where ph_locn = :curlocn;
       endsr;

       begsr opnphcursor;
       exec sql
        open phcursor;
       endsr;

       begsr clsphcursor;
       exec sql
        close phcursor;
       endsr;

       begsr updthist;
       exec sql
        update k_prodhis
          set ph_locn = :newlocn
          where current of phcursor;
       endsr;
      /end-free
