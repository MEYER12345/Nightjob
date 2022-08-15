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
      **   Name: K3S_9701
      **   Type: ILE RPG Program
      **   Desc: Users signed on to K3S-Replenish end job (Batch)
      **
      *****************************************************************
      **
      **  This program is used to end the active users in K3S-Replenish
      **
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/29/2014.
      **  Remarks. Rewrote program in RPG free.
      **
      *****************************************************************

     d jobend          s             10    inz('*K3S_END* ')
     d index           s              2  0
     d nbrusers        s              4  0                                      location
     d endoption       s              1a
     d spaceattr       s             10
     d spacevalue      s              1
     d spacetext       s             50
     d spaceauth       s             10    inz('*ALL')
     d spacereplc      s             10    inz('*YES')
     d spacelen        s              9b 0 inz(1024)
     d fileformat      s              8    inz('OBJL0100')
     d filelib         s             20    inz('K3S_0010FM*LIBL     ')
     d objecttype      s             10    inz('*FILE     ')
     d membername      s             10    inz('*NONE     ')
     d startposit      s              9b 0
     d startlen        s              9b 0
     d savestart       s              9b 0
     d savelen         s              9b 0
     d*
     d errords         ds                  inz
     d bytesprovd              1      4b 0 inz(116)
     d bytesavail              5      8b 0
     d messageid               9     15
     d err###                 16     16
     d messagedta             17    116
     d*
     d inputds         ds                  inz
     d userspace                     20
     d spacename                     10    overlay(userspace:1)
     d                                     inz('ACTIVEUSER')
     d spacelib                      10    overlay(userspace:11)
     d                                     inz('QTEMP')
     d genheadds       ds                  inz
     d inputsize             113    116b 0
     d listoffset            125    128b 0
     d numberlist            133    136b 0
     d entrysize             137    140b 0
     d*
     d listds          ds
     d jobname                 1     10
     d jobuser                11     20
     d job#                   21     26
     d*
     d CRTUSRSPC       PR                  EXTPGM('QUSCRTUS')
     d  userspace                          like(userspace)
     d  spaceattr                          like(spaceattr)
     d  spacelen                           like(spacelen)
     d  spacevalue                         like(spacevalue)
     d  spaceauth                          like(spaceauth)
     d  spacetext                          like(spacetext)
     d  spacereplc                         like(spacereplc)
     d  errords                            like(errords)
     d*
     d CRTACTUSL       PR                  EXTPGM('QWCLOBJL')
     d  userspace                          like(userspace)
     d  fileformat                         like(fileformat)
     d  filelib                            like(filelib)
     d  objecttype                         like(objecttype)
     d  membername                         like(membername)
     d  errords                            like(errords)
     d*
     d RTVHDRINFO      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  genheadds                          like(genheadds)
     d*
     d RTVDTLINFO      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  inputds                            like(inputds)
     d*
     d RTVDTINLOG      PR                  EXTPGM('QUSRTVUS')
     d  userspace                          like(userspace)
     d  startposit                         like(startposit)
     d  startlen                           like(startlen)
     d  listds                             like(listds)
     d*
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      *
      /free
       //-------------------------------------------------- Once Routine
       //once routine
         exsr $_once;

       //-------------------------------------------------- End of Main Loop

       //finished, set on LR
         *inlr = *on;

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Once routine

         begsr $_once;
       // Create user space in qtemp called ACTIVEUSER
         spaceattr = *blanks;
         spacevalue = *blanks;
         spacetext = *blanks;
         callp CRTUSRSPC (userspace:
                          spaceattr:
                          spacelen:
                          spacevalue:
                          spaceauth:
                          spacetext:
                          spacereplc:
                          errords);

       // Create active user list in ACTIVEUSER for K3S_0010FM
         callp CRTACTUSL (userspace:
                          fileformat:
                          filelib:
                          objecttype:
                          membername:
                          errords);

         spacename = 'ACTIVEUSER';
         spacelib   ='QTEMP     ';
       //retrieve for user area header information
         startposit = 1;
         startlen = 140;
         callp RTVHDRINFO (userspace:
                           startposit:
                           startlen:
                           genheadds);
         startlen = inputsize;
         spacename = 'ACTIVEUSER';
         spacelib   ='QTEMP     ';

        //retrieve for user detail information
         startposit = 1;
         callp RTVDTLINFO (userspace:
                           startposit:
                           startlen:
                           inputds);
         startposit = (listoffset +1);
         startlen = entrysize;
         savelen = entrysize;
         spacename = 'ACTIVEUSER';
         spacelib   ='QTEMP     ';
         nbrusers   = numberlist;


       //retrieve for user area detail information for all logged entries
         For index = 1 to nbrusers;
             spacename = 'ACTIVEUSER';
             spacelib ='QTEMP     ';
             savestart = startposit;
             callp RTVDTINLOG (userspace:
                               startposit:
                               startlen:
                               listds);
       //Set LDA for user being shut down
             callp K3S_9031 (jobuser);

       //Close Guest Book Entry
             callp K3S_9900 (jobend:job#);

             endoption = '4';
             callp K3S_9701CL (jobname:
                               jobuser:
                               job#:
                               endoption);
             startposit = (savestart + savelen);
         endfor;
         endsr;
      /end-free
