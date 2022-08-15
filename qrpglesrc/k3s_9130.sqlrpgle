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
      **   Name: K3S_9130
      **   Type: ILE RPG Program
      **   Desc: Remove alt source entries in K_INTALTR file for 1 company
      **
      *****************************************************************
      **
      **  This program will remove all alternate source entries for
      **  a company.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 04/04/2014.
      **  Remarks. Changed to use SQL DELETE statement to delete
      **           K_INTALTR records where ia_comp = comp.
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/31/2014.
      **  Remarks. Added /COPY compiler directive to utilize prototype
      **           source member.
      *****************************************************************
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed
     d K3S_9130        PI
     d  comp                          1
      * ---------------------------------------------------------------
      /free
       exec sql
        set option commit = *none;
       exec sql
        Delete from k_intaltr
          Where ia_comp = :comp;

       *inlr = '1';
      /end-free
