      *****************************************************************
     h copyright('(C) Copyright 1996 - 2010 King III Solutions, Inc.  +
     h Rel 5.05 2010-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2010 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9120
      **   Type: ILE RPG Program
      **   Desc: Remove products records for alt srce entries for 1 comp
      **
      *****************************************************************
      **
      **  This program will remove all product records that were
      **  generated for alternate source purposes. An OPNQRYF has
      **  prepared file k_product, to only include records for
      **  a specific company, which are pr_altsrce = 1 get deleted.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 04/03/2014.
      **  Remarks. Changed program to utilize an SQL DELETE statement
      **           to delete from k_product those records where
      **           pr_comp = :comp and pr_atlsrce = 1.  Then removed
      **           code from OPNQRYF in CL program K3S_9110CL that
      **           selected records where pr_comp = comp and pr_altsrce
      **           = 1. The SQL delete statement will handle the selection
      **           as well as the deletion.
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/31/2014.
      **  Remarks. Added /COPY compiler directive to utilize
      **           prototype source.
      *****************************************************************
      * products
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9120        PI
     d  comp                          1
      *
      /free
       exec sql
        set option commit = *none;
       exec sql
        Delete from k_product
          Where pr_comp = :comp and
                pr_altsrce = 1;

       *inlr = *on;
      /end-free
