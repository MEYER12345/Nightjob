     fqprint    o    f  132        printer oflind(*inof)
     d MyDS            ds
     d   comp                              LIKE(pq_comp)
     d   buyr                              LIKE(pq_buyr)
     d   regn                              LIKE(pq_regn)
     d   locn                              LIKE(pq_locn)
     d   supl                              LIKE(pq_supl)
     d*
     d   first15       S             15
     d*
     d prodsoq       e ds                  ExtName(k_prodsoq)
     d*
     c/exec sql
     c+ declare mainCursor Cursor
     c+   for
     c+ select pq_comp, pq_buyr, pq_regn, pq_locn, pq_supl
     c+   from k_prodsoq
     c+   where pq_comp = 'C' and pq_buyr = '03'
     c/end-exec
     c/exec sql
     c+ open mainCursor
     c/end-exec
     c/exec sql
     c+ fetch next
     c+  from mainCursor
     c+  into :myDS
     c/end-exec
     c                   dow       SQLSTT = '00000'
     c*    buyr          dsply
     c                   except    detail
     c/exec sql
     c+ fetch next
     c+  from mainCursor
     c+  into :myDS
     c/end-exec
     c                   enddo
     c                   except    footer
     c                   eval      *inlr = *on
     oqprint    e            detail         1  1
     o                       comp                 7
     o                       buyr                18
     o                       regn                28
     o                       locn                38
     o                       supl                48
     o          e            footer            1
     o                                              '*********'
