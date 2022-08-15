      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 4.41 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')
     h*
     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
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
      **   Name: K3S_9023
      **   Type: ILE RPG Program
      **   Desc: Nite job - capture timestamp for end of K3S_1500
      **
      *****************************************************************
      **
      **  This program would run just after K3S_1500 and provide an
      **  ending timestamp that can be used to gather the possible
      **  K_LOGPROD records that designate LG_LOGTYPE = '4' for
      **  deleted products.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 08/14/2014.
      *   Remarks. Changed program to call subprocedure K3S_Retrieve_
      *            Time_Stamp to get time stamp and use %char RPG
      *            built-in function to convert time stamp to character.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_9023        PI
     d  end_1500                     26

      /free
       //call subprocedure to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       end_1500 = %char(time_stamp);

       //finished, set on LR
       *inlr = *on;
      /end-free

