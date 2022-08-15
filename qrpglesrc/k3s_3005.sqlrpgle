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
      **   Name: K3S_3005
      **   Type: ILE RPG Program
      **   Desc: Remove temporary product records at night
      **
      *****************************************************************
      **
      **  This program will remove all product records that were
      **  generated with PR_TEMPORY = 1 during the day. An OPNQRYF has
      **  prepared file k_product, to only include records for
      **  a specific company, which are pr_tempory = 1 get deleted.
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 04/14/2014.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_3005
      *            from K3S_NITECL leaving the call to RPG program
      *            K3S_3005 and passing the company code as a parameter.
      *            In K3S_3005 program added code to use an SQL delete
      *            statement to delete the k_product records where
      *            PR_COMP is equal to the passed company parameter and
      *            PR_TEMPORY = 1.
      *****************************************************************
      * products
      * ------------------------------------------------------parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_3005        PI
     d  comp                          1a
      * ---------------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                closqlcsr = *endactgrp;
       exec sql
        Delete
          From k_product
          Where pr_comp = :comp and
                pr_tempory = 1;
       //
       *inlr = '1';
      /end-free
