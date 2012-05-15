library( grid )
library(annmap)

annmapConnect( 'pb-test' )

files  = list( 'Asyn_0mm', 'Syn0_0mm',     list( 'Asyn_0mm', 'Syn0_0mm' ) )
total.reads = c( 690174, 1880786,       690174 + 1880786 )
.labels = list( bquote( sigma * "value   "), '2nd Sample   ', bquote( Delta * "value   ") ) 

.genes = data.frame( stable_id=c( 'SPAC8E11.03c','SPAC3C7.03c','SPBC32H8.06' ),
                     symbol   =c( 'dmc1',        'rhp55',      'mug93'       ),
                     chr      =c( 'I',           'I',          'II'          ),
                     start    =c( 3380288,       2063075,      1460675       ),
                     end      =c( 3386088,       2068875,      1466475       ) )

.regions = list(
  data.frame( start    =c( 3381288, 3381288, 3385000 ),
              end      =c( 3381388, 3381488, 3386000 ),
              strand   =c( 1,       1,       -1 ),
              name     =c( 'danny1', 'danny2', 'antidanny' ) ),
  data.frame(),
  data.frame()
)

danny.normalisation = function( rle.object, sample.read.count, width ) {
  .val = log2( ( 1e10 * ( runValue( rle.object ) / ( sample.read.count * width ) ) ) + 0.00001 )
  # Clamp to zero (can end up with negatives from normalisation)
  runValue( rle.object ) = ifelse( .val < 0, 0, .val )
  rle.object
}

grid.newpage()

ngenes = length( as.character( .genes$stable_id ) )
pushViewport(viewport(layout=grid.layout(1, ngenes, widths=unit( 1.0 / ngenes, "npc"), heights=unit(1, "npc"), just=c('left','bottom'))))

invisible( lapply( seq_along( .genes$stable_id ), function( idx ) {
  pushViewport( viewport(layout.pos.col=idx, layout.pos.row=1) )
  row = .genes[ idx, ]
  data = lapply( seq_along( files ), function( fidx ) {
    f = files[[ fidx ]]
    .fn = paste( '/groups/acbb/to.keep/tyates/acbbutil_example_files/', f, '.bam', sep='' )
    .rle = convertBamToRle( .fn, as.character(row$chr), row$start, row$end, chr.name.mapping=function(n) { paste( 'EG:', n, sep='' ) } )
    .read.count = total.reads[ idx ]
    list( rle=list( '+'=danny.normalisation( .rle$'+', .read.count, row$end - row$start ), '-'=danny.normalisation( .rle$'-', .read.count, row$end - row$start ) ),
          col=rainbow(length( as.character( .genes$stable_id ) ), v=0.5, s=0.5)[ fidx ],
          name=.labels[[ fidx ]] )
  } )
  .sym = as.character( row$symbol )
  .chr = as.character( row$chr )
  ngsBridgePlot( as( data.frame( space=.chr, start=row$start, end=row$end ), 'RangedData' ),
                  data=data,
                  highlights=.regions[[idx]],
                  main=bquote( italic( .(.sym) ) * ' (chr' * .(.chr) * ')' ),
                  probe.plot=NULL,
                  trace.label.properties=NULL,
                  gene.area.height=3,
                  drop.partial=T )
  popViewport()
} ) )
popViewport()