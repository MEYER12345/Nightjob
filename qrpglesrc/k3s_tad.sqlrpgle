    ‚ *****************************************************************
     h copyright('(C) Copyright 1996 - 2015 King III Solutions, Inc.  +
     h Rel 5.2  2015-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')
    ‚
    ‚ *****************************************************************
    ‚ **
    ‚ **   K3S-Replenish (R) - Inventory REPLENISHment System
    ‚ **   Copyright (C) 1996-2015 by King III Solutions, Inc.
    ‚ **   Program property of King III Solutions, Inc.
    ‚ **   All rights reserved.
    ‚ **   K3S_Replenish (R) is a Registered Trade Mark of
    ‚ **   King III Solutions Inc.
    ‚ **
    ‚ *****************************************************************
    ‚ **
    ‚ **   Name: K3S_TAD
    ‚ **   Type: ILE RPG Program
    ‚ **   Desc: TAD adjustment - twice a decade
    ‚ **
    ‚ *****************************************************************
    ‚ **
    ‚ **   This program will clear the accumulated demand fields in
    ‚ **   the K_PRODUCT file, for both weekly and 13-four weekly
    ‚ **   forecasted products. It is used twice a decade, every 5
    ‚ **   years. Past years were 2009 and 2014. Next use will be
    ‚ **   during the 1st week of 2019.
    ‚ **
    ‚ *****************************************************************
      /free
       exec sql
        set option commit = *none;
       exec sql
        declare product_cursor cursor
          for
         select pr_accsale, pr_accouts, pr_accdem
           from k_product
             where pr_forcint = 52 or pr_forcint = 13
               for update of pr_accsale, pr_accouts, pr_accdem;
       exec sql
        open product_cursor;
       exec sql
        fetch next
         from product_cursor;
       If SQLSTT = '24501';
         exec sql
          open product_cursor;
       endif;
       Dow SQLSTT = '00000';
          exec sql
            update k_product
            set pr_accsale = 0, pr_accouts = 0, pr_accdem = 0
              where current of product_cursor;
          exec sql
            fetch next
              from product_cursor;
       enddo;
       exec sql
        close product_cursor;
       *inlr = '1';
      /end-free
