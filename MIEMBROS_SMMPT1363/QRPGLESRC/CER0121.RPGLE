000100101025     F*********************************************************************
000200101025     F*                                                                   *
000300101025     F* PROGRAMA CER0121  - GENERA ARCHIVO DE EXTRACTO Y COMPENSACION     *
000400101025     F*                                                                   *
000500101025     F* PROGRAMADO POR    - FERNANDO GUTIERREZ M.                         *
000600101025     F*                                                                   *
000700101025     F* FECHA PROGRAMACION- DIC/1991                                      *
000800101025     F*                                                                   *
000900120924     F* FECHA REVISION    - SEP/2012 JORGE HERNANDO CERCADO ZAMORA        *
001000101025     F*                                                                   *
001100101025     F*                     ABRIL 1993 ELECTRON                           *
001200101025     F*                     JULIO 1993 TARJETA CAFETERA                   *
001300101025     F*                     AGOST 1993 SPDH TERMINAL XL                   *
001400101025     F*                     AGOST 93  CCDTYP 21 Y CCDMSG 220 NO GRABA     *
001500101025     F*                     FEBRE 94  MVTOS MULTICOLOR Y REDEBAN          *
001600101025     F*                     MARZO 94  PROPINAS T6 Y T7                    *
001700101025     F*                               TABLA ALTERNA DE REDES              *
001800101025     F*                     DIC   94  AGENCIAS DE VIAJES                  *
001900101025     F*                     MARZO 95  SERVIBANCA                          *
002000101025     F*                     JULIO 95  TRANSACCIONES DE PRUEBA SWFTESTE    *
002100101025     F*                     JULIO 95  AUDIORESPUESTA                      *
002200101025     F*                     DICIE 00  SERVICIOS PUBLICOS                  *
002300180924     F*                                                                   *
002400120924     F* TODOS LOS DERECHOS RESERVADOS POR CREDIBANCO - 2012               *
002500101025     F*********************************************************************
002600190129     ?***--------------------------------------------------------------------***
002700190221     ?*PROYECTO            : MPT-014 TOKENIZACION FASE 1                       *
002800190129     ?*MODIFICO            : Carlos Arturo Castro V.  - Esp. desarrollo AS400  *
002900190221     ?*FECHA MODIFICACION  : 2018-12-05 - ID = 001                             *
003000190129     ?*DESCRIPCION         : Se ajusta para recibir trx tokenizadas            *
003100190129     ?***--------------------------------------------------------------------***
003200190129     ?*PROYECTO            : BUGS-229 Error alineacion tarjeta extracto        *
003300190129     ?*MODIFICO            : Carlos Arturo Castro V.  - Esp. desarrollo AS400  *
003400190221     ?*FECHA MODIFICACION  : 2019-01-29 - ID = 002                             *
003500190129     ?*DESCRIPCION         : Las tarjetas de longitud 17 no se alinean co-     *
003600190129     ?*                      rrectamente.                                      *
003700190129     ?***--------------------------------------------------------------------***
003800190226     ?*PROYECTO            : MPT-510 SMB - Decreto 2230 propina no sea su-     *
003900190226     ?*                      jeta a lisquidacion de valores del int. adq.      *
004000190226     ?*MODIFICO            : Carlos Arturo Castro Vasquez                      *
004100190226     ?*FECHA MODIFICACION  : 2019-02-21 - ID = 003                             *
004200190226     ?*DESCRIPCION         : Se elimina rutina PROPIN para no recacalcular el  *
004300190226     ?*                      valor de la compra si la propina supera el 15%    *
004400190221     ?***--------------------------------------------------------------------***
004500190808     ?*PROYECTO            : MPT-1363 Ajuste liquidacion propina por tipo de   *
004600190808     ?*                      terminal para que se refleje en el DEPO.          *
004700190808     ?*MODIFICO            : Carlos Arturo Castro Vasquez                      *
004800190808     ?*FECHA MODIFICACION  : 2019-08-08 - ID = 004                             *
004900190808     ?*DESCRIPCION         :                                                   *
005000190808     ?*                                                                        *
005100190808     ?***--------------------------------------------------------------------***
005200180924      *
005300140707     HFIXNBR(*ZONED) DATEDIT(*YMD) ALWNULL(*USRCTL) INDENT('| ')
005400070808     F*           ARCHIVO DE ENTRADA / EXTRACTO TANDEM
005500160808     FSWFEXTRA  IP   F 3945        DISK
005600131106     F*           ---> ARCHIVO DE ENTIDADES / FIID
005700131106     FSIF0100F01IF   E           K DISK    PREFIX(F_)
005800131106     F*           ---> ARCHIVO DE ENTIDADES / BINES
005900131106     FSIF0100B03IF   E           K DISK    PREFIX(B_)
006000160808     F*           ---> ARCHIVO DE ENTIDADES / BINES
006100160808     FSIF0100B02IF   E           K DISK    PREFIX(C_)
006200160808     F                                     RENAME(SIR0100B:SIR0102B)
006300180829     F*           ---> ARCHIVO DE ENTIDADES / BINES
006400180829     FSIF0100B04IF   E           K DISK    PREFIX(D_)
006500180829     F                                     RENAME(SIR0100B:SIR0104B)
006600131106     F*           BINES-BANCOS
006700961018     FPLL0100B  IF   E           K DISK
006800961018     F*           TABLA DE BANCOS
006900961018     FRELTA01A  IF   E           K DISK
007000961018     F*           TABLA DE BANCOS POR COMPENSACION
007100961018     FREFTAB01  IF   E           K DISK
007200961018     F                                     RENAME(RERTAB01:RERBANCO)
007300961018     F*           TABLA DE DESCUENTOS
007400961018     FREFTAB06  IF   E           K DISK
007500961018     F*           TABLA DE BINES
007600961018     FREFTAB08  IF   E           K DISK
007700961018     F*           TABLA DE TERMINALES
007800961018     FRELTA21A  IF   E           K DISK
007900961018     F*           TABLA DE TERMINALES CODIGO TERMINAL PARA VALIDACION
008000961018     FRELTA21B  IF   E           K DISK
008100961018     F                                     RENAME(RERTAB21:RERCODTE)
008200961018     F*           ARCHIVO DE ESTABLECIMIENTOS
008300961018     FESFBASIC  IF   E           K DISK
008400961018     F*           ARCHIVO DE DESCUENTOS Y TARIFAS
008500961018     FESFDESCU  IF   E           K DISK
008600961018     F*           ARCHIVO DE CODIGOS DE AEROLINEAS
008700961018     FESFAEREO  IF   E           K DISK
008800961018     F*           ARCHIVO DE CODIGOS DE ENCADENAMIENTO DE AEROLINEAS
008900961018     FREFTAB68  IF   E           K DISK
009000041202     F*           ARCHIVO DE CODIGOS DE ENCADENAMIENTO DE AEROLINEAS
009100041202     FRELTA068  IF   E           K DISK
009200041202     F                                     RENAME(RERTAB68:RERAVIAT)
009300961018     F*           ARCHIVO DE CODIGOS EQUIVALENTES EN LAS REDES
009400961018     FESFREDES  IF   E           K DISK
009500961018     F*           ARCHIVO DE BINES INTERNALES
009600970826     FSIF0052A  IF   E           K DISK
009700980915     F*           ARCHIVO DE COMERCIOS DINNERS
009800980915     FASDCOMER  IF   E           K DISK
009900980915     F                                     RENAME(ASRCOMER:ASRDINNE)
010000961018     F*           ARCHIVO DE COMERCIOS
010100961018     FASLCOM02  IF   E           K DISK
010200001205     F*           CUENTAS SERVICIOS PUBLICOS
010300071114     FPFFBASIC  IF   E           K DISK
010400110301     F*           TABLA DE COMERCIOS - TARJETA PREFERENCIAL O COMPARTIDA
010500110301     FESFCOMTP  IF   E           K DISK    PREFIX(I_)
010600130529     F*           TABLA DE COMERCIOS DE EXCEPCION LAPOLAR
010700130529     FESF1020C  IF   E           K DISK    PREFIX(J_)
010800180829     F*           TABLA MIEMBROS PRINCIPALES VISA
010900180829     FBAFTAB40  IF   E           K DISK
011000961018     F*           ARCHIVO DE TRANSACCIONES DE OTRAS REDES EN TER ASCRE
011100980901     FSWFREDES  O    E           K DISK
011200990709     F*           ARCHIVO DE TRANSACCIONES REVERSADAS
011300990709     FSWFREVER  UF A E           K DISK
011400961018     F*           ARCHIVO DE OPERACIONES
011500980901     FSWFOPERA  O    E           K DISK
011600010521     F*           ARCHIVO DE OPERACIONES XRA MULTICOLOR/DINNER CON IVA
011700010521     FSWFIVART  O    E           K DISK
011800180924     F*           ARCHIVO DE TOKENS
011900001205     FSWFTKHST  O    E           K DISK
012000961018     F*           ARCHIVO DE OPERACIONES DE PRUEBA
012100980901     FSWFTESTE  O    E           K DISK
012200961018     F*           ARCHIVO DE CANJE
012300980806     FCEF0500A  O  A E           K DISK
012400110301     F*           ARCHIVO DE TRANS. NO COMPENSADAS BINES PREFERENC.
012500110301     FCEF0503W  O  A E           K DISK
012600961018     F*           ARCHIVO DE TRANSACCIONES ADQUIRIDAS OTRAS REDES
012700961018     F*           SIN CONSIGNAR
012800980901     FSWFSINDP  O  A E           K DISK
012900961018     F*           ARCHIVO DE REGISTROS CON DUENO DE TERMINAL PARA CONCI
013000980901     FSWFOWTER  O  A E           K DISK
013100961018     F*           RUTA SWFCAPTU TARJETA APROBACION PARA SPDH
013200980806     FCEL0500A  UF   E           K DISK
013300980806     F                                     RENAME(CER0500A:SWRCAPT1)
013400961018     F*           RUTA SWFCAPTU TARJETA APROBACION PARA SPDH
013500980806     FCEL0500B  UF   E           K DISK
013600980806     F                                     RENAME(CER0500A:SWRCAPT2)
013700961018     F*           RUTA SWFCAPTU TARJETA CODES CTA Y VALTO PARA PROPINAS
013800980806     FCEL0500C  UF   E           K DISK
013900980806     F                                     RENAME(CER0500A:SWRCAPT3)
014000961018     F*           ARCHIVO DE CANJE REGISTROS ELIMINADOS
014100030622     FSWFELIMI  UF A E           K DISK
014200961018     F*           ARCHIVO DE CANJE ELIMINADOS POR TERMINAL ERRADO
014300030622     FSWFPOSER  UF A E           K DISK
014400961018     F*           ARCHIVO AUXILIAR DE INFORMACION BASICA
014500980901     FSWFBAS01  O    E           K DISK
014600180829     F*           REFERENCIA CITIBANK /VISA
014700180829     FREFTAB8001IF   E           K DISK
014800180829      *           TIPO TERMINALES AGREGADORES
014900180829     FREFTAB38AGIF   E           K DISK    PREFIX(R_)
015000190808      *           TERMINALES PERMITEN PROPINA
015100190808 004 FRELTAB25  IF   E           K DISK
015200190808      *           MCC PERMITEN PROPINA
015300190808 004 FRELTAB26  IF   E           K DISK
015400100126     D*
015500100126     D*           ---> INDICADORES
015600100126     D*
015700100126     DICHKDES          S              1    INZ(*ZEROS)
015800110124     DICHKPFF          S              1    INZ(*ZEROS)
015900110301     DICHKCTP          S              1    INZ(*ZEROS)
016000130529     DICHK20C          S              1    INZ(*ZEROS)
016100131106     DICHKFID          S              1    INZ(*ZEROS)
016200150831     DICHKBAS          S              1    INZ(*ZEROS)
016300131106     DICHK100          S              1    INZ(*ZEROS)
016400160808     DICHK102          S              1    INZ(*ZEROS)
016500130529     DWADQPOL          S              1    INZ(*BLANKS)
016600190129 002 DWTALON           S              2S 0 INZ(0)
016700100126     D*
016800961018     D*           ARREGLO PARA CALCULO DE PROPINAS
016900961018     D* CAMPO QUE LLEGA
017000961018     D AR              S              1    DIM(10)
017100961018     D* DATO NUMERICO
017200961018     D DAT             S              1    DIM(10)
017300961018     D*
017400961018     D* CUENTAS AEROLINEAS
017500961018     D AER             S             18    DIM(10)
017600961018     D*
017700980915     D* NUMERO DE TARJETA DINNERS
017800980915     D NUM             S              1    DIM(19)
017900980915     D* NUMERO DE CUENTA  DINNERS
018000980915     D CTA             S              1    DIM(19)
018100980915     D CNU             S              1    DIM(19)
018200001205     D*
018300961018     D DFETRA          DS
018400961018     D  DDIATR                 1      2  0
018500961018     D  DMESTR                 3      4  0
018600961018     D  DANOTR                 5      6  0
018700961018     D*
018800961018     D XFEPRO          DS
018900961018     D  DANOCA                 1      2  0
019000961018     D  DMESCA                 3      4  0
019100961018     D  DDIACA                 5      6  0
019200130619     D*
019300980806     D XFHCON          DS
019400980806     D  XANORE                 1      2  0
019500980806     D  XMESRE                 3      4  0
019600980806     D  XDIARE                 5      6  0
019700961018     D*
019800961018     D WFECOM          DS
019900961018     D  DANOCO                 1      2  0
020000961018     D  DMESCO                 3      4  0
020100961018     D  DDIACO                 5      6  0
020200961018     D*
020300961018     D WCDTAR          DS
020400961018     D  WBINCO                 1      6  0
020500961018     D  WTARJE                 7     19
020600961018     D*
020700961018     D WCDTA1          DS
020800961018     D  WTARJ1                 1     13
020900961018     D  WFILL1                14     19
021000961018     D*
021100961018     D WCDTA2          DS
021200961018     D  WTARJ2                 1     16
021300961018     D  WFILL2                17     19
021400020912     D*
021500961018     D WCDRE6          DS
021600961018     D  WFILRE                 1      1
021700961018     D  WCDRE1                 1      6
021800961018     D  WCDRE2                 7      8
021900961018     D  WCDRE3                 2      7
022000961018     D*
022100961018     D WCDEST          DS
022200961121     D  WPOSUN                 1      1
022300961121     D  WCOUNI                 2      9
022400961018     D  WFILER                 1      1  0
022500961018     D  WCDES1                 2      7  0
022600961018     D  WFILE2                 7      7
022700961121     D  WFILE5                 8      9
022800961018     D  WFILE3                 8     10
022900961018     D  WCAFET                 2     10
023000961018     D  WCDES2                 7     10
023100961018     D*
023200961018     D WCODAE          DS
023300961018     D  WCODA2                 1      3  0
023400970524     D  WAER06                 4      9
023500970524     D  WAER08                 4     11
023600970524     D  WCODA3                10     11
023700961018     D*
023800961018     D WNUTER          DS
023900101006     D  WNUTE1                 1      8
024000961018     D  WNUTE2                 9     10
024100961018     D  WCLIPP                 1      4
024200961018     D*
024300961018     D WCTADE          DS
024400961018     D  WCOMPE                 1      2  0
024500961018     D  WCUENT                 3     18
024600961018     D*
024700961018     D MCDEST          DS
024800961018     D  BFILER                 1      4
024900970317     D  BCOD02                 1      2
025000970317     D  BCOD04                 1      4
025100961018     D  BCODES                 1     10  0
025200961018     D*
025300961018     D DNADA3          DS
025400961018     D  WNADA4                 1      5
025500970225     D  WAGEUN                 6      9
025600961018     D  WAGENC                 6     15
025700961018     D  WADQUI                17     17
025800961018     D  WNADA5                18     20
025900130619     D*
026000961018     D* CAMBIO DE DICIEMBRE 1995
026100961018     D* EL CODIGO WCDAER SE UNIRA AL CODIGO DE CIUDAD DE LA AGENCIA
026200961018     D* PARA DETERMINAR EL CODIGO DE LA AEROLINEA
026300961018     D CAEREO          DS
026400961018     D  WCUOTA                 1      2
026500961018     D  WCDAER                 3      4  0
026600001205     D  WAUT06                 4      9
026700001205     D  WAUT08                 4     11
026800961018     D  WFILAE                 5     11  0
026900130619     D*
027000980806     D WREFER          DS
027100980806     D  DBINRE                 1      6  0
027200980806     D  DDIARE                 7      8  0
027300980806     D  DMESRE                 9     10  0
027400980806     D  DANORE                11     12  0
027500980806     D  XSECUE                13     21  0
027600020226     D*
027700020226     D WTIPNE          DS
027800020226     D  MCBTAR                 1      2  0
027900020226     D  MAUTOR                 3      4  0
028000110223     D*
028100110301     DWVRWRK           S             12  0 INZ(*ZEROS)
028200110301     DWFLD02           S             02    INZ(*ZEROS)
028300110301     DWFLD06           S             06    INZ(*ZEROS)
028400131106     DWTPPRO           S             02    INZ(*ZEROS)
028500150831     DWINDCB           S             09    INZ(*ZEROS)
028600130619     DWVRIAC           S             12  0 INZ(*ZEROS)
028700161111 fc  DWciu             S              7A   INZ(*BLANKS)
028800161115     D Ptipo           S              1A   INZ('2')
028900161115     D Pcodig          S              5A   INZ('00002')
029000161115     D Ptxtori         S             50A   INZ(*BLANKS)
029100161115     D Ptxtdep         S             50A   INZ(*BLANKS)
029200161115     D Pind            S              1A   INZ(*BLANKS)
029300110124     D*
029400181004 001 D wTarjetaTK      S             19A   INZ(*BLANKS)
029500181004 001 D wIndtoken       S              1A   INZ(*BLANKS)
029600181008 001 D wFinpgm         S              1A   INZ(*BLANKS)
029700181004      *
029800180924 001 D wDSOPERA      E DS                  EXTNAME(SWFOPERA) PREFIX(O_)
029900180924 001 D wDSEXTRA      E DS                  EXTNAME(SWFEXTRA) PREFIX(E_)
030000180924 001 D wINDFIN         S              1A
030100130619     D*
030200110124     DWSTARP           S             02  0 INZ(*ZEROS)
030300110124     DWLNFLD           S             02  0 INZ(*ZEROS)
030400110124     DWENDPS           S             02  0 INZ(16)
030500110124     D*
030600161115     **
030700161115     DDepura_Carac     PR                  ExtPgm('SIR0087')
030800161115     D Ptipo                          1A
030900161115     D Pcodig                         5A
031000161115     D Ptxtori                       50A
031100161115     D Ptxtdep                       50A
031200161115     D Pind                           1A
031300161115     **
031400180924 001 DGEN_TOKEN        PR                  ExtPgm('CER0170')
031500180924 001 D wFOPERA                             LIKE(wDSOPERA)
031600180924 001 D wFEXTRA                             LIKE(wDSEXTRA)
031700180924 001 D wINDFIN                        1A
031800181004      *
031900181004 001 DVAL_TOKEN        PR                  Extpgm('CER0172')
032000181004 001 D wTarjetatk                    19A
032100181004 001 D wIndtoken                      1A
032200181004 001 D wFinpgm                        1A
032300180924     **
032400990709     ISWRREVER
032500990709     I              MVRTRA                      FVRTRA
032600030622     ISWRELIMI
032700030622     I              DNRFAC                      FNRFAC
032800030622     ISWRPOSER
032900030622     I              DNRFAC                      TNRFAC
033000961018     IASRCOMER
033100961018     I              MNUFAX                      XNUFAX
033200961018     I              MNUTER                      YNUTER
033300001205     I              MCDCIU                      YCDCIU
033400980915     IASRDINNE
033500980915     I              MNUFAX                      ONUFAX
033600980915     I              MNUTER                      ONUTER
033700001205     I              MCDCIU                      OCDCIU
033800961018     I*
033900961020     ISWFEXTRA  DD  36   13NCT   14NCH
034000961018     I                                  3   21  CDATTI            36
034100961018     I*TIPO REGISTRO
034200961018     I                                 22   23  CRECTY            36
034300961018     I*HERRAN ENE.12.94                      24  31 CFIL01    36
034400961018     I                                 24   27  CFIID1            36
034500961018     I                                 28   31  CCOMPE            36
034600030425     I                                 32   32  CTAR11            36
034700961018     I                                 32   59  CTARJE            36
034800961018     I                                 60   62  CMEMBR            36
034900961018     I                                 63   78  CFIL02            36
035000961018     I                                 63   66  CADQUI            36
035100961018     I                                 67   70  CCREDA            36
035200961018     I                                 79   97  CCODES            36
035300961018     I                                 79   86  CCODE1            36
035400961018     I                                 79   88  CCORED            36
035500050913     I                                 80   97  CREDEB            36
035600970225     I                                 80   87  CAGENC            36
035700970314     I                                 87   87  WPRUEB            36    55
035800000608     I                                 98  105  CTEREX            36
035900961018     I                                 98  113  CTERMI            36
036000961018     I                                114  120  CFIL03            36
036100961018     I* CODIGO MENSAJE
036200961218     I                                121  124  CCDMES            36
036300961018     I                                125  126  CSTAMS            36
036400961018     I                                127  127  CORIGE            36
036500961018     I                                128  128  CRESPO            36
036600961018     I                                129  130  CFIL04            36
036700961018     I                                131  149  CENTIM            36
036800961018     I                                150  168  CEXTIM            36
036900961018     I                                169  187  CREENT            36
037000961018     I                                188  193  CFETRA            36
037100961018     I                                194  201  CHOTRA            36
037200961018     I                                202  207  CFECON            36
037300961018     I                                208  219  CFIL06            36
037400961018     I                                220  231  CSECUE            36
037500961018     I                                232  256  CDIRES            36
037600961018     I                                257  278  COWTER            36
037700961018     I                                279  291  CCITTY            36
037800020424     I                                300  300  CINDPR            36
037900040615     I                                301  301  CINAC2            36
038000020424     I                                329  330  CTIPTE            36
038100961018     I                                292  336  CFIL07            36
038200961018     I                                337  340  CGRCRT            36
038300961018     I                                341  348  CIDCRT            36
038400020226     I                                349  352  CTIPNE
038500961018     I                                349  360  CFIL08            36
038600961018     I                                361  362  CCDTYP            36
038700961018     I                                363  363  CTPCAR            36
038800961018     I                                364  365  CCDCTA            36
038900961018     I                                366  366  CCDCAT            36
039000961018     I                                367  368  CFIL09            36
039100961018     I                                369  396  CTARJ1            36
039200961018     I                                397  399  CCDRES            36
039300961018     I                                400  418  CMONTO            36
039400961018     I                                419  437  CAJUST            36
039500961018     I                                438  441  CVECTO            36
039600961018     I                                442  529  CFIL10            36
039700130906     I                                510  519  CFL10B            36
039800130906     I                                520  529  CFL10C            36
039900961018     I                                531  545  CSYMAU            36
040000961018     I                                546  552  CFIL11            36
040100961018     I*NUMERO DE AUTORIZACION
040200961018     I                                553  560  CREFER            36
040300961018     I                                561  561  CLONGI            36
040400961018     I                                562  569  CREFE2            36
040500961018     I                                570  593  CFIL12            36
040600961018     I*CAPTURA O AUTHOR ONLY
040700961018     I                                594  594  CCDCAP            36
040800961018     I                                595  595  CFIL13            36
040900961018     I                                596  597  CCDAJU            36
041000000814     I                                601  602  CCAM25            36
041100030213     I                                601  602  CPCCOD            36
041200961018     I                                603  605  CVCASH            36
041300030213     I                                603  604  CPEMOD            36
041400030213     I                                603  604  CPSEMD            36
041500030213     I                                605  605  CPTCAP            36
041600961018     I                                598  672  CFIL14            36
041700030213     I                                677  687  CCAICD            36
041800980320     I                                677  682  CBAVAR            36
041900961018     I                                673  683  CAEREO            36
042000001205     I                                683  683  CFLGSP            36    58
042100961018     I                                684  729  CCFI14            36
042200961018     I                                730  730  CCDFLG            36
042300961018     I                                731 1584  CFIL15            36
042400961018     I*CAMPO DE CAPTURA ALINEA A LA IZQUIERDA
042500010406     I* OJO OJO DESPLAZAMIENTO PRO MULT    15661575 CPROPI    36
042600010406     I                               1566 1574  CPROPI            36
042700010402     I                               1628 1639  CVRIVA            36
042800040615     I                               1640 1651  CVRIBS            36
042900040615     I                               1652 1663  CVRDON            36
043000040615     I                               1595 1762  CFIL16            36
043100040615     I                               1763 1784  CTOKSP            36
043200040615     I                               1785 1786  CCDCIU            36
043300040615     I                               1787 1788  CCDESP            36
043400040615     I                               1789 1836  CNRFAC            36
043500040615     I                               1837 1856  CNRFAX            36
043600040615     I                               1857 1869  CNIESP            36
043700040615     I                               1870 1884  CUNESP            36
043800040615     I                               1885 1903  CMONXX            36
043900040615     I                               1904 1911  CVCFAC            36
044000040615     I                               1912 1912  CFGPAG            36
044100040615     I                               1913 1937  CINTER            36
044200040615     I                               1938 1941  CTELEC            36
044300040615     I                               1942 1944  CCVV2X            36
044400040615     I                               1945 1955  CCEDUL            36
044500040615     I                               1956 1956  CFGLEN            36
044600040615     I                               1957 1957  CUSFLG            36
044700040615     I                               1958 1967  CTOKCE            36
044800040615     I                               1968 1971  CFGVAL            36
044900040615     I                               1972 1972  CESTRE            36
045000040615     I                               1973 1975  CCONRE            36
045100040615     I                               1976 1985  CPOSTA            36
045200040615     I                               1986 1986  CFGCEL            36
045300040615     I                               1987 1987  CTPTCI            36
045400040615     I                               1988 1988  CINDAD            36
045500040615     I                               1989 1989  CCVDPR            36
045600040615     I                               1990 1993  CCAMUS            36
045700040615     I                               1994 2003  CTOKC6            36
045800040615     I                               2004 2043  CIDIDT            36
045900040615     I                               2044 2083  CTRSET            36
046000040615     I                               2084 2093  CTOKC7            36
046100040615     I                               2094 2125  CSERTA            36
046200040615     I                               2126 2135  CTOKC8            36
046300040615     I                               2136 2167  CSERCO            36
046400040615     I                               2168 2177  CTOK23            36
046500040615     I                               2178 2259  CTRAC1            36
046600170411     I                               2191 2192  CIDGRE            36
046700040615     I                               2260 2269  CTOK23            36
046800040615     I                               2270 2281  CVRDIS            36
046900040615     I                               2282 2293  CVRACI            36
047000040615     I                               2294 2299  CUSDAT            36
047100061106     I                               2300 2309  CTHDQ7            36
047200061106     I                               2302 2303  CINDQ7            36
047300061106     I                               2310 2325  CPRDES            36
047400061106     I                               2326 2329  CDPCQ7            36
047500061106     I                               2330 2340  CFLDQ7            36
047600061106     I                               2339 2340  CADQQ7            36
047700061106     I                               2341 2361  CUSRQ7            36
047800070328     I                               2362 2371  CTOKQ8            36
047900070328     I                               2372 2379  CPOSFS            36
048000070328     I                               2380 2380  CCERQ8            36
048100070328     I                               2381 2388  CESTFS            36
048200070328     I                               2389 2389  CUSRQ8            36
048300070328     I                               2390 2421  CTOKQ9            36
048400070328     I                               2422 2431  CTOKQB            36
048500070328     I                               2432 2440  CAGEQB            36
048600070328     I                               2441 2451  CUSRQB            36
048700070808     I* TOKEN PAGO POR PARTES
048800070808     I                               2452 2461  CHDRQP            36
048900070808     I                               2462 2473  CNCREP            36
049000070808     I                               2474 2483  CNCEDP            36
049100070808     I                               2484 2495  CVCREP            36
049200070808     I                               2496 2507  CVIVAP            36
049300070808     I                               2508 2519  CVBIVP            36
049400070808     I                               2520 2521  CNCUOP            36
049500070808     I                               2522 2529  CFH1CP            36
049600070808     I                               2530 2531  CBLK2P            36
049700070808     I* TOKEN CORRESPONSALES NO BANCARIOS
049800070808     I                               2532 2541  CHDRQC            36
049900070808     I                               2542 2542  CINDPC            36
050000070808     I                               2543 2558  CNTARC            36
050100070808     I                               2559 2567  CBLK9C            36
050200091118     I* TOKEN'S B2, B3, B4, B5 Y BJ PARA EMV
050300091118     I                               2568 2735  CTQNB2            36
050400091118     I                               2736 2825  CTQNB3            36
050500091118     I                               2826 2855  CTQNB4            36
050600091118     I                               2856 2903  CTQNB5            36
050700091118     I                               2904 3173  CTQNB6            36
050800091118     I                               3174 3265  CTQNBJ            36
050900120317     I* TOKEN'S B0-B1 - TID AMEX
051000120317     I                               3266 3333  CTQNB0            36
051100120317     I* TOKEN'S B17 - TID VISA
051200120317     I                               3334 3367  CTQN17            36
051300180924     I* REGISTRO COMPLETO SWFEXTRA
051400180924 001 I                                  1 3945  CREGEX            36
051500961018     I*
051600980924     C     *IN99         DOWEQ     '0'
051700961018     C*
051800961231     C     *ENTRY        PLIST
051900961231     C     XFECAN        PARM                    XFECAN            6
052000961018     C*
052100961018     C     KEYELI        KLIST
052200961018     C                   KFLD                    KTARJE           19
052300961018     C                   KFLD                    KCODES           10 0
052400961018     C                   KFLD                    KNUMCO            4 0
052500961018     C                   KFLD                    KVALTO            9 0
052600961018     C*
052700961018     C     KEYEL2        KLIST
052800961018     C                   KFLD                    KTARJE           19
052900961018     C                   KFLD                    KCODES           10 0
053000961018     C                   KFLD                    KCTADE           18
053100961018     C                   KFLD                    KVALTO            9 0
053200961018     C*
053300961018     C     KEYEL1        KLIST
053400961018     C                   KFLD                    KTARJE           19
053500961018     C                   KFLD                    DCODAU            6
053600961018     C*
053700961018     C     KEYAJU        KLIST
053800961018     C                   KFLD                    DTARJE           19
053900961018     C                   KFLD                    DCODAU            6
054000961018     C*
054100961018     C     KEYBIN        KLIST
054200961018     C                   KFLD                    KBININ            9 0
054300961018     C*
054400961018     C     KEYVAL        KLIST
054500961018     C                   KFLD                    TCODES           10 0
054600101006     C                   KFLD                    TNUMTE            5
054700961018     C*
054800961018     C     KEYAER        KLIST
054900961018     C                   KFLD                    MCIUDA
055000961018     C                   KFLD                    WCDAER
055100961018     C*
055200041202     C     KEYAE2        KLIST
055300041202     C                   KFLD                    KAEROL           10 0
055400041202     C                   KFLD                    KCIUDA            5 0
055500041202     C*
055600110124     C     KEYPFF        KLIST
055700001205     C                   KFLD                    KESVIS           10 0
055800110124     C                   KFLD                    KCDVIS           04 0
055900131106     C*
056000131106     C     KEYFID        KLIST
056100131106     C                   KFLD                    F_DCDFID
056200131106     C*
056300131106     C     KEY100        KLIST
056400131106     C                   KFLD                    B_DCDTAB
056500131106     C                   KFLD                    B_DCDCMP
056600131106     C                   KFLD                    B_DCDBIN
056700160808     C*
056800160808     C     KEY100C       KLIST
056900160808     C                   KFLD                    C_DCDTAB
057000160808     C                   KFLD                    C_DCDBIN
057100180829     C*
057200180829     C     KEY100D       KLIST
057300180829     C                   KFLD                    D_DCDTAB
057400180829     C                   KFLD                    D_DCDBIN
057500180829     C                   KFLD                    D_DCDFID
057600110223     C*
057700110301     C     KEYCTP        KLIST
057800110301     C                   KFLD                    I_MCDCMP
057900110301     C                   KFLD                    I_MCDBIN
058000110301     C                   KFLD                    I_MCODES
058100001205     C*
058200130529     C     KEY20C        KLIST
058300130529     C                   KFLD                    J_MCDCMP
058400130529     C                   KFLD                    J_MCDBIN
058500130529     C                   KFLD                    J_MCODES
058600130529     C*
058700961018     C                   MOVE      '1'           *IN99
058800001205     C*
058900990709     C  N99              READ      SWRREVER                               33
059000030622     C  N99              READ      SWRELIMI                               33
059100030622     C  N99              READ      SWRPOSER                               33
059200961018     C                   Z-ADD     *ZEROS        WTOTAL            8 0
059300961018     C                   Z-ADD     *ZEROS        WPNTJE            4 4
059400961018     C                   Z-ADD     *ZEROS        WVALDE           11 0
059500961018     C                   Z-ADD     *ZEROS        WPORDT            4 2
059600101006     C                   MOVE      *ZEROS        XNUTER            5
059700101006     C                   MOVE      *ZEROS        WNUMTE            5
059800961018     C                   Z-ADD     *ZEROS        WCOMPE            2 0
059900961018     C                   Z-ADD     *ZEROS        KCODES           10 0
060000961018     C                   Z-ADD     *ZEROS        WCODES           10 0
060100961018     C                   Z-ADD     *ZEROS        KVALTO            9 0
060200961018     C                   Z-ADD     *ZEROS        MPROPI           10 0
060300961018     C                   MOVE      *BLANKS       MDATPR           10
060400961018     C                   Z-ADD     *ZEROS        WCOMIS           11 0
060500961018     C                   Z-ADD     *ZEROS        WVALEX            6 0
060600961018     C                   Z-ADD     *ZEROS        WCALPR            6 0
060700961018     C                   MOVE      *BLANKS       WCDEST           10
060800961018     C                   MOVE      *BLANKS       WBINBU            6
060900961018     C                   MOVE      *BLANKS       KTARJE           19
061000961018     C                   MOVE      *BLANKS       KCTADE           18
061100961018     C                   MOVE      *ZEROS        KNUMCO
061200961231     C                   MOVE      XFECAN        MENCAD
061300961018     C                   MOVE      *ZEROS        WWRK01            5 0
061400961018     C                   MOVE      '0'           WNOGRA            1
061500041202     C                   Z-ADD     *ZEROS        WNRREG
061600130319AC   C                   EVAL      *IN69 = '0'
061700961018     C*
061800961018     C                   END
061900181004      *
062000181004      * Valida si transacciones es tokenizada para no procesar.
062100181004 001 C                   EVAL      wTarjetatk = %Subst(CTARJE:1:19)
062200181008 001 C                   EVAL      wIndtoken  = *Blanks
062300181008 001 C                   EVAL      wFinpgm    = 'N'
062400181128     C                   If        wTarjetatk <> *Blanks  and  CFIID1 = 'VISA'
062500181128     C                             and CCOMPE = 'VISA'
062600181004 001 C                   CALLP     VAL_TOKEN(wTarjetatk: wIndtoken: wFinpgm)
062700181008     C                   Endif
062800181128      *
062900181008 001 C                   If        wIndtoken  <> '1'
063000961018     C*
063100050302     C                   MOVE      'CV'          DFRANQ
063200961018     C                   ADD       1             WTOTAL            8 0
063300981105     C                   EXSR      CELPOI
063400001205     C     CCAM25        IFEQ      '15'
063500001205     C                   MOVEL     CTERMI        WTER              3
063600001205     C     WTER          IFEQ      'CLI'
063700001205     C     WTER          OREQ      'ASY'
063800001205     C     WTER          OREQ      'ZTN'
063900001205     C     WTER          OREQ      'TRM'
064000001205     C                   MOVE      *BLANKS       CTERMI
064100001205     C                   MOVEL     '00099904'    CTERMI
064200001205     C                   END
064300001205     C                   END
064400961018     C                   MOVE      COWTER        MOWTER
064500961018     C                   MOVEL     CTARJE        WCDTAR
064600961018     C                   MOVE      CAEREO        WCODAE           11
064700030425     C*
064800030425     C     CTAR11        IFEQ      '4'
064900030425     C     CCOMPE        ANDEQ     '0023'
065000030425     C                   MOVE      '9223'        CCOMPE
065100030425     C                   END
065200030425     C*
065300970216     C*
065400961018     C     CCDMES        IFEQ      '0220'
065500961018     C     CCDTYP        ANDEQ     '21'
065600961018     C                   MOVE      '1'           WNOGRA
065700961018     C                   ELSE
065800961018     C                   MOVE      '0'           WNOGRA
065900961018     C                   END
066000970216     C*
066100961018     C     CRECTY        IFNE      '04'
066200961018     C     CCDMES        ANDNE     '0320'
066300961018     C     CCDMES        ANDNE     '0500'
066400961018     C     CCDMES        ANDNE     '0000'
066500961018     C     CTARJE        ANDNE     *BLANKS
066600961018     C     CTARJE        ANDNE     *ZEROS
066700961018     C     WBINCO        ANDNE     000088
066800961018     C                   MOVE      'P'           MDISPO
066900961018     C                   MOVEL     CTARJE        WCDTAR
067000961018     C                   MOVEL     CTARJE        WCDTA1
067100961018     C                   MOVEL     CTARJE        WCDTA2
067200100514     C*
067300100514     C*--> TRANS. DAVIVIENDA MSG 0220 Y 0221 SOLO PARA MONITOREO PRM.(PRMD)
067400100514     C*
067500100514     C                   IF        WBINCO = 400558 OR
067600100514     C                             CADQUI = 'PRMD'
067700100514     C                   MOVE      '1'           *IN25
067800100514     C                   ELSE
067900100514     C                   MOVE      '0'           *IN25
068000100514     C                   END
068100190129     C*
068200190129     C* Calcula long. tarjeta y alinea a la derecha con blancos a la izq.
068300190129 002 C                   Evalr     MCDTAR = %Trim(WCDTA1)
068400190129 002 C                   Evalr     MCTAAF = %Trim(WCDTA1)
068500190129 002 C                   Eval      WTALON = %Len(%Trim(WCDTA1))
068600020912     C*                                                              |
068700961018     C                   MOVE      WBINCO        WCBCLA            6 0
068800961018     C                   MOVE      WBINCO        XBINCO            6
068900961018     C*                                                              !
069000961018     C                   EXSR      CHKBIN
069100961018     C*                                                              !
069200000713     C                   Z-ADD     WBANCO        MCDCMP
069300961018     C*                                                              !
069400961018     C* IF CCOMPE IFEQ 901        HERRAN ENE.12.94                   !
069500961018     C* MOVE CCOMPE TO MCDCMP                                        !
069600961018     C* END                                                          !
069700961018     C*                                                              !
069800110910     C*
069900110910     C*    EXCEPCIONES FIDID Y/O BINES: POR NO ENCONTRADO EN PLF0100A
070000961018     C     *IN80         IFEQ      '1'
070100110910     C*
070200961018     C     CCOMPE        IFEQ      '0901'
070300961018     C                   MOVE      CCOMPE        MCDCMP
070400961018     C                   END
070500961018     C*                                                             !!
070600961018     C     CCOMPE        IFEQ      '9901'
070700961018     C                   MOVE      CCOMPE        MCDCMP
070800961018     C                   END
070900961018     C*                                                             !!
071000961018     C     CCOMPE        IFEQ      'REDE'
071100961018     C                   Z-ADD     902           MCDCMP
071200961018     C                   END
071300961018     C*                                                             !!
071400961018     C     CCOMPE        IFEQ      '0902'
071500961018     C                   MOVE      CCOMPE        MCDCMP
071600961018     C                   END
071700961018     C*                                                            !!!
071800961018     C     CCOMPE        IFEQ      '0903'
071900961018     C                   MOVE      CCOMPE        MCDCMP
072000961018     C                   END
072100961018     C*                                                             !!
072200961018     C     CCOMPE        IFEQ      '9034'
072300961018     C                   MOVE      CCOMPE        MCDCMP
072400961018     C                   END
072500961018     C*                                                             !!
072600961018     C     CCOMPE        IFEQ      '0034'
072700961018     C                   MOVE      CCOMPE        MCDCMP
072800961018     C                   END
072900000912     C*
073000000912     C     CCOMPE        IFEQ      '8001'
073100000912     C                   MOVE      0089          MCDCMP
073200000912     C                   END
073300021121     C     CCOMPE        IFEQ      '8002'
073400021121     C                   MOVE      0088          MCDCMP
073500021121     C                   END
073600040217     C     CCOMPE        IFEQ      '8006'
073700040217     C                   MOVE      0087          MCDCMP
073800040217     C                   END
073900050429     C     CCOMPE        IFEQ      '8902'
074000050429     C                   MOVE      0086          MCDCMP
074100050429     C                   END
074200080129     C*/
074300080129     C*/LOS BINES PROCESSA Y BONO REGALO 799911 799912 606125 -----.!!
074400080129     C*/SE REPORTAN CON EL FIID 8207 PARA NO SER COMPENSADOS  -----.!!
074500080129     C*/
074600050915     C     CCOMPE        IFEQ      '8107'
074700051020     C     CCOMPE        OREQ      '8108'
074800071206     C     CCOMPE        OREQ      '8207'
074900050915     C                   MOVE      0085          MCDCMP
075000050915     C                   END
075100081002     C*
075200100316     C*    FID 8015  TARJETA ASSENDA FAMILIAS EN ACCION
075300080808     C     CCOMPE        IFEQ      '8015'
075400080808     C                   MOVE      0083          MCDCMP
075500080808     C                   END
075600080825     C*
075700110426     C*    FID 4110  TARJETA PRIVADA FALABELLA (BIN: 627180)
075800111129     C*    FID 5110  TARJETA MASTER  FALABELLA E/DIRECTO (BIN:514332-528209)
075900090122     C     CCOMPE        IFEQ      '4110'
076000111129     C     CCOMPE        OREQ      '5110'
076100090122     C                   MOVE      0068          MCDCMP
076200090122     C                   END
076300100803     C*
076400110426     C*    FID 8069  TARJETA PRIVADA COOMEVA (BIN: 590710)
076500100803     C     CCOMPE        IFEQ      '8069'
076600100803     C                   MOVE      0069          MCDCMP
076700100803     C                   END
076800090122     C*
076900110426     C*    FID 8102  TARJETA PRIVADA DIVERCITY (BIN: 799902)
077000060526     C     CCOMPE        IFEQ      '8102'
077100060526     C                   MOVE      0084          MCDCMP
077200060526     C                   END
077300101025     C*
077400110426     C*    FID 8016  TARJETA PRIVADA LAPOLAR (BIN: 636307)
077500130529     C*
077600101025     C     CCOMPE        IFEQ      '8016'
077700101025     C                   MOVE      0082          MCDCMP
077800101025     C                   END
077900101216     C*
078000110426     C*    FID 8209  TARJETA PRIVADA CAD. EXITO (BIN: 570423) - T. LATUYA
078100101216     C     CCOMPE        IFEQ      '8209'
078200101216     C                   MOVE      0070          MCDCMP
078300101216     C                   END
078400110301     C*
078500110426     C*    FID 8019  TARJETA PRIVADA COLPATRIA (BIN: 606374) - B. PREFERENC.
078600110301     C     CCOMPE        IFEQ      '8019'
078700110301     C                   MOVE      0019          MCDCMP
078800110301     C                   END
078900110426     C*
079000110426     C*    FID 0047  TARJETA PRIVADA CREDIVALORES (BIN: 636549)
079100110426     C     CCOMPE        IFEQ      '0047'
079200110426     C                   MOVE      0047          MCDCMP
079300110426     C                   END
079400110910     C*
079500110910     C*    FID 5019  TARJETA MASTER  COLPATRIA E/DIRECTO (BIN: 589526)
079600110910     C     CCOMPE        IFEQ      '5019'
079700110910     C                   MOVE      0019          MCDCMP
079800110910     C                   END
079900120102     C*
080000120102     C*    FID 8077  TRANS. BIG PASS              (BIN: 636215)
080100120102     C     CCOMPE        IFEQ      '8077'
080200120102     C                   MOVE      8077          MCDCMP
080300120102     C                   END
080400120102     C*
080500130523     C*    FID 8088  TRANS. FEDCO (BANKVISION)    (BIN: 636870)
080600130523     C     CCOMPE        IFEQ      '8088'
080700130523     C                   MOVE      8088          MCDCMP
080800130523     C                   END
080900131114     C*
081000131114     C*    FID 8086  TRANS. ASOPAGOS CAJASAN      (BIN: 630923)
081100131114     C     CCOMPE        IFEQ      '8086'
081200131114     C                   MOVE      8086          MCDCMP
081300131114     C                   END
081400140707     C*
081500140707     C*    FID 8004  TRANS. COOGUASIMALES         (BIN: 637028)
081600140707     C     CCOMPE        IFEQ      '8004'
081700140707     C                   MOVE      8004          MCDCMP
081800140707     C                   END
081900130523     C*
082000110301     C*
082100990602     C                   MOVEL     XBINCO        XVISAS            1
082200101025     C*
082300990602     C     XVISAS        IFEQ      '4'
082400990602     C                   Z-ADD     90            MCDCMP
082500990602     C                   END
082600101025     C*
082700021029     C                   MOVEL     MCDTAR        XFRANQ            5
082800101025     C*
082900021029     C     XFRANQ        IFEQ      '    3'
083000021029     C                   MOVE      '91'          MCDCMP
083100021029     C                   END
083200021021     C     CFIL09        IFEQ      'AX'
083300021021     C                   Z-ADD     91            MCDCMP
083400021021     C                   END
083500101025     C*
083600961018     C                   END
083700101025     C*
083800961018     C                   Z-ADD     1             MCLAUT
083900101025     C*
084000961018     C     CCOMPE        IFEQ      '0901'
084100961018     C     CCREDA        OREQ      '0901'
084200961018     C                   Z-ADD     2             MCLAUT
084300961018     C                   END
084400961018     C*                                                              !
084500961018     C     CCOMPE        IFEQ      '9901'
084600961018     C     CCREDA        OREQ      '9901'
084700961018     C                   Z-ADD     2             MCLAUT
084800961018     C                   END
084900961018     C*                                                              !
085000130319AC   C                   EVAL      *IN69 = '0'
085100961018     C     CCOMPE        IFEQ      'REDE'
085200961018     C     CCREDA        OREQ      'REDE'
085300961018     C                   Z-ADD     3             MCLAUT
085400130319AC   C                   EVAL      *IN69 = '1'
085500961018     C                   END
085600961018     C*                                                              !
085700961018     C     CCOMPE        IFEQ      '0902'
085800961018     C     CCREDA        OREQ      '0902'
085900961018     C                   Z-ADD     3             MCLAUT
086000961018     C                   END
086100961018     C*                                                              !
086200961018     C     CCOMPE        IFEQ      '0903'
086300961018     C     CCREDA        OREQ      '0903'
086400961018     C                   Z-ADD     4             MCLAUT
086500961018     C                   END
086600961018     C*                                                              !
086700961018     C     CCOMPE        IFEQ      '0034'
086800961018     C     CCREDA        OREQ      '0034'
086900961018     C                   Z-ADD     5             MCLAUT
087000961018     C                   END
087100961018     C*                                                              !
087200961018     C     CCOMPE        IFEQ      '9034'
087300961018     C     CCREDA        OREQ      '9034'
087400961018     C                   Z-ADD     5             MCLAUT
087500961018     C                   END
087600961018     C*                                                              !
087700961018     C     CCOMPE        IFEQ      'VISA'
087800961018     C     CCREDA        ANDEQ     'VISA'
087900961018     C                   Z-ADD     9             MCLAUT
088000961018     C                   END
088100961018     C*                                                              !
088200961018     C                   MOVEL     CMEMBR        MCDMBR
088300001205     C                   MOVE      CCAM25        MCDMBR
088400961018     C*                                                              !
088500990429     C*                    MOVELWBINCO    WBININ                     !
088600990429     C*                    MOVE '000'     WBININ                     !
088700990429     C*                    MOVE WBININ    KBININ                     !
088800990429     C*          KBININ    CHAINSIR0052A             50              !
088900961018     C*                                                              !
089000990429     C*          *IN50     IFEQ '0'                        ---------.!
089100990429     C*          RLONGI    IFNE '* '                       --------.!!
089200990429     C*                    MOVE RLONGI    WLONGI  20               !!!
089300990429     C*          WTALON    IFNE WLONGI                     -------.!!!
089400990429     C*                    Z-ADD90        MCDCMP                  !!!!
089500990429     C*                    END                             -------.!!!
089600990429     C*                    END                             --------.!!
089700990429     C*                    END                             ---------.!
089800961018     C*                                                              !
089900961018     C                   Z-ADD     *ZEROS        KBININ
090000961018     C                   Z-ADD     *ZEROS        WTALON
090100961018     C*                                                              !
090200961018     C* COMERCIOS MULTICOLOR                                         !
090300961018     C                   MOVE      '0'           WESRED            1
090400961218     C                   MOVE      *BLANKS       WCORED           16
090500961218     C                   MOVEL     CCORED        WCORED
090600961018     C*                                                              !
090700961018     C     CCORED        CHAIN     ESRREDES                           22
090800060321     C                   MOVE      '1'           *IN22
090900961018     C     *IN22         IFEQ      '0'
091000961216     C* MODIF PARA TENER EN CUENTA EL CODIGO UNICO CONVENIO INTEREDS!!
091100961216     C     RCODES        IFLE      999999
091200961216     C                   MOVE      *BLANKS       CCODES
091300961216     C                   MOVE      *BLANKS       CCODE7            7
091400961216     C                   MOVE      RCODES        CCODE7
091500961216     C                   MOVEL     '0'           CCODE7
091600961216     C                   MOVEL     CCODE7        CCODES
091700961216     C                   END
091800961216     C     RCODES        IFGT      999999
091900961216     C                   MOVE      *BLANKS       CCODES
092000961216     C                   MOVE      *BLANKS       CCODE9            9
092100961216     C                   MOVE      RCODES        CCODE9
092200961216     C                   MOVEL     '0'           CCODE9
092300961216     C                   MOVEL     CCODE9        CCODES
092400961217     C                   END
092500961018     C*                                                             !!
092600961018     C     RREDID        IFEQ      901
092700961018     C                   MOVE      '2'           WADQUI
092800961217     C*PARA CONVERTIR EL REGISTRO COMO CAPTURA DE DATOS            !!!
092900961217     C     CCDCAP        IFEQ      '0'
093000961217     C                   MOVE      '1'           CCDCAP
093100961217     C                   END
093200961217     C                   END
093300961217     C*                                                             !!
093400961018     C     RREDID        IFEQ      902
093500961217     C                   MOVE      '0'           CCDCAP
093600961018     C                   MOVE      '3'           WADQUI
093700961018     C                   END
093800961018     C*                                                             !!
093900961018     C                   ELSE
094000961018     C*                                                             !!
094100961018     C     WCORED        CHAIN     ASRCOMER                           22
094200060321     C                   MOVE      '1'           *IN22
094300961018     C     *IN22         IFEQ      '0'
094400961217     C     WCORED        ANDNE     *BLANKS
094500961216     C*                                                            !!!
094600961216     C                   Z-ADD     *ZEROS        WCCOCV           10 0
094700961216     C                   MOVEL     MCCOCV        WCCOCV
094800961216     C     WCCOCV        IFLE      999999
094900961216     C                   MOVE      *BLANKS       CCODES
095000961216     C                   MOVE      *BLANKS       CCODE7
095100961217     C                   Z-ADD     WCCOCV        WCODE7            7 0
095200961018     C                   MOVE      WCODE7        CCODE7            7
095300961018     C                   MOVEL     '0'           CCODE7
095400961018     C                   MOVEL     CCODE7        CCODES
095500961216     C                   END
095600961216     C     WCCOCV        IFGT      999999
095700961216     C                   MOVE      *BLANKS       CCODES
095800961216     C                   MOVE      *BLANKS       CCODE9
095900961217     C                   Z-ADD     WCCOCV        WCODE9            9 0
096000961216     C                   MOVE      WCODE9        CCODE9            9
096100961216     C                   MOVEL     '0'           CCODE9
096200961216     C                   MOVEL     CCODE9        CCODES
096300961216     C                   END
096400961216     C*                                                            |!!
096500961217     C                   MOVE      '0'           CCDCAP
096600961018     C                   MOVE      '1'           WESRED
096700961216     C                   MOVE      '3'           WADQUI
096800961216     C*                                                            |!!
096900961216     C                   ELSE
097000961018     C*                                                            |!!
097100961018     C                   MOVE      '1'           WADQUI
097200961018     C*                                                            |!!
097300961018     C                   END
097400961018     C*                                                             !!
097500961018     C                   END
097600961018     C* COMERCIOS MULTICOLOR                                         !
097700961018     C                   MOVEL     CCODES        WCDEST
097800050913     C*                                                              |
097900050913     C     CCREDA        IFEQ      'REDE'
098000050913     C                   MOVEL     CREDEB        WCDEST
098100050913     C                   END
098200961018     C*                                                              |
098300961018     C     WFILE2        IFEQ      *BLANKS
098400961018     C                   MOVE      WCDEST        MCDEST
098500961018     C                   ELSE
098600961018     C                   MOVE      WCDES1        MCDEST
098700961018     C                   END
098800961018     C*                                                              |
098900961121     C*  PARA TENER EN CUENTA EL CODIGO UNICO                        !
099000961121     C*  FERNANDO GUTIERREZ  9611                                    |
099100961121     C*                                                              |
099200150831     C                   IF        WPOSUN = '0' OR
099300150831     C                             WPOSUN = '1' AND
099400150831     C                             WFILE5 <> *BLANKS
099500150831     C                   MOVE      WCOUNI        MCDEST
099600150831     C                   ENDIF
099700961121     C*                                                              |
099800961018     C* CODIGO DE ESTABLECIMIENTO PARA AEROLINEAS                    !
099900961018     C*                                                              !
100000961018     C                   MOVEL     CTERMI        MNUTER
100100961018     C*                                                              !
100200091001     C*    --->  EL PRODUCTO VISA CASH ESTA DESCONTINUADO
100300091001     C*    --->  SE SUSPENDEN LAS LINEAS CONTINUACION PARA
100400091001     C*    --->  ACTIVA LA FUNCIONALIDAD DEL PRODUCTO EMV O CHIP
100500091001     C*
100600091001     C*    CVCASH        IFEQ      '051'
100700091001     C*                  MOVEL     'CASH'        MNUTER
100800091001     C*                  END
100900961018     C*                                                              |
101000961018     C                   MOVE      MNUTER        WNUTER
101100101006     C                   MOVE      WNUTE1        WNUMTE
101200961018     C*                                                              |
101300961018     C     WCLIPP        IFNE      'CLIP'
101400961018     C     WCLIPP        ORNE      'CASH'
101500130319     C*
101600130319     C*    --->          IN69 = TRANS. ADQ. X REDEBAN/NO DEBE ENTRAR CHKT21
101700130319     C*
101800130319AC   C                   IF        *IN69 = '0'
101900130319     C                   EXSR      CHKT21
102000130319AC   C                   ENDIF
102100130319     C*
102200090923     C*    /Se adiciono terminal tipo 6 para que funcione como una tipo 8
102300090923     C*    /de acuerdo a requerimiento de la DMA (Oscar Romero-Carlos Lindo
102400090923     C*    /Agosto 2009
102500961018     C     MTIPTE        IFEQ      8
102600090923     C     MTIPTE        OREQ      6
102700961018     C                   MOVE      '1'           *IN23
102800961018     C                   ELSE
102900961018     C                   MOVE      '0'           *IN23
103000961018     C                   END
103100961018     C                   END
103200961018     C*                                                              |
103300961018     C* CODIGO DE ESTABLECIMIENTO PARA AUDIORESPUESTA                !
103400961018     C*                                                              !
103500961018     C     MTIPTE        IFEQ      9
103600961018     C                   MOVE      '1'           *IN26
103700961018     C                   ELSE
103800961018     C                   MOVE      '0'           *IN26
103900961018     C                   END
104000961018     C*                                                              !
104100980320     C* CODIGO DE ESTABLECIMIENTO PARA BAVARIA                       !
104200980320     C*                                                              !
104300980320     C     MTIPTE        IFEQ      10
104400980320     C                   MOVE      '1'           *IN27
104500980320     C                   ELSE
104600980320     C                   MOVE      '0'           *IN27
104700980320     C                   END
104800980320     C*                                                              !
104900001205     C* CODIGO DE ESTABLECIMIENTO PARA SERVICIOS PUBLICOS            !
105000001205     C*                                                              !
105100001205     C     CCAM25        IFEQ      '15'
105200001205     C                   MOVE      '1'           *IN28
105300001205     C                   ELSE
105400001205     C                   MOVE      '0'           *IN28
105500001205     C                   END
105600001205     C*                                                              !
105700961018     C* SI IN23 ES 1 SIGNIFICA UNA TRANSACCION DE AEROLINEA          |
105800961018     C*                                                              |
105900961018     C     *IN23         IFEQ      '1'
106000961018     C*                                                             ||
106100970314     C                   MOVE      *BLANKS       WAGE06            6
106200970314     C                   MOVE      *BLANKS       WAGE08            8
106300970314     C                   MOVE      *ZEROS        WAGENC
106400970314     C*                                                             ||
106500970317     C     *IN55         IFEQ      '1'
106600970314     C                   MOVEL     CAGENC        WAGE06
106700970314     C                   MOVE      WAGE06        WAGENC
106800970314     C                   ELSE
106900970314     C                   MOVEL     CAGENC        WAGE08
107000970314     C                   MOVE      WAGE08        WAGENC
107100970314     C                   END
107200961018     C*                                                             ||
107300981106     C                   MOVE      WAGENC        WCDAGE           10 0
107400981106     C*                                                             ||
107500981106     C     WCDAGE        CHAIN     ESRBASIC                           41
107600961018     C     KEYAER        CHAIN     RERTAB68                           42
107700961018     C*                                                             ||
107800961018     C     *IN42         IFEQ      '0'
107900961018     C                   MOVE      AEROLI        WCDES1
108000961018     C                   MOVE      AEROLI        MCDEST
108100970317     C     BCOD04        IFEQ      '0000'
108200970317     C                   MOVEL     '    '        WCDES1
108300970317     C                   MOVEL     '    '        MCDEST
108400970317     C                   END
108500970317     C     BCOD02        IFEQ      '00'
108600970317     C                   MOVEL     '  '          WCDES1
108700970317     C                   MOVEL     '  '          MCDEST
108800970317     C                   END
108900961018     C                   ELSE
109000961018     C                   MOVE      00020         MCIUDA
109100961018     C     KEYAER        CHAIN     RERTAB68                           42
109200961018     C     *IN42         IFEQ      '0'
109300961018     C                   MOVE      AEROLI        WCDES1
109400961018     C                   MOVE      AEROLI        MCDEST
109500970317     C     BCOD04        IFEQ      '0000'
109600970317     C                   MOVEL     '    '        WCDES1
109700970317     C                   MOVEL     '    '        MCDEST
109800970317     C                   END
109900970317     C     BCOD02        IFEQ      '00'
110000970317     C                   MOVEL     '  '          WCDES1
110100970317     C                   MOVEL     '  '          MCDEST
110200970317     C                   END
110300961018     C                   ELSE
110400961018     C                   MOVE      *BLANKS       WCDES1
110500961018     C                   MOVE      *BLANKS       MCDEST
110600961018     C                   END
110700961018     C                   END
110800961018     C*                                                             ||
110900961018     C                   END
111000961018     C*                                                              |
111100961018     C* AUDIORESPUESTA                                               |
111200961018     C*                                                              |
111300970225     C     *IN26         IFEQ      '1'
111400970524     C     WCODA3        IFEQ      '  '
111500970524     C                   MOVE      WAER06        WCDES1
111600970524     C                   MOVE      WAER06        MCDEST
111700970524     C                   ELSE
111800970524     C                   MOVE      WAER08        WCDES1
111900970524     C                   MOVE      WAER08        MCDEST
112000970524     C                   END
112100970314     C                   MOVE      *BLANKS       WAGE06            6
112200970314     C                   MOVE      *BLANKS       WAGE08            8
112300970314     C                   MOVE      *ZEROS        WAGENC
112400970318     C     *IN55         IFEQ      '1'
112500970314     C                   MOVEL     CAGENC        WAGE06
112600970314     C                   MOVE      WAGE06        WAGENC
112700970314     C                   ELSE
112800970314     C                   MOVEL     CAGENC        WAGE08
112900970314     C                   MOVE      WAGE08        WAGENC
113000970314     C                   END
113100970225     C                   END
113200961018     C*                                                              !
113300980320     C* BAVARIA                                                      !
113400980320     C     *IN27         IFEQ      '1'
113500980320     C                   MOVE      *ZEROS        WAGENC
113600040514     C*                    MOVE CBAVAR    WAGENC                     |
113700980320     C                   END
113800980320     C*                                                              !
113900001205     C* SERVICIOS PUBLICOS
114000001205     C     *IN28         IFEQ      '1'
114100001205     C*                                                             ||
114200001205     C                   MOVE      *ZEROS        WAGENC
114300001205     C*                                                             ||
114400001205     C     *IN58         IFEQ      '1'
114500001205     C                   MOVE      WAUT06        WAGENC
114600001205     C                   ELSE
114700001205     C                   MOVE      WAUT08        WAGENC
114800001205     C                   END
114900001205     C*                                                             ||
115000001205     C                   END
115100001205     C*                                                              !
115200070328     C* MULTICOMERCIO                                                |
115300070328     C     MTIPTE        IFEQ      15
115400070328     C                   MOVE      *ZEROS        WAGENC
115500070328     C                   MOVE      CESTFS        WAGENC
115600070328     C                   END
115700070328     C*                                                              !
115800961121     C     WPOSUN        IFEQ      '5'
115900961121     C     WPOSUN        OREQ      '6'
116000961121     C     WPOSUN        OREQ      '7'
116100961018     C                   MOVE      WCAFET        MCDEST
116200961018     C                   END
116300961018     C*                                                              |
116400961018     C                   MOVEL     CCDMES        MCDMSG
116500130619     C*
116600961018     C                   MOVEL     CFECON        MFHCON
116700991215     C                   Z-ADD     19            MMCINT
116800991215     C     MFHCON        IFLT      500000
116900991215     C                   Z-ADD     20            MMCINT
117000991215     C                   END
117100961018     C                   MOVEL     CRESPO        MCDORI
117200130619     C*
117300961018     C                   MOVEL     CFETRA        MFHTRA
117400991215     C                   Z-ADD     19            MMCNAL
117500991215     C     MFHTRA        IFLT      500000
117600991215     C                   Z-ADD     20            MMCNAL
117700991215     C                   END
117800130619     C*
117900961018     C                   MOVEL     CHOTRA        MHOTRA
118000961018     C                   MOVEL     CSECUE        MSETRA
118100961018     C                   MOVEL     CCDTYP        MTPTRA
118200961018     C                   MOVEL     CTPCAR        MTPTHJ
118300961018     C                   MOVEL     CCDCTA        MTPCTA
118400961018     C                   MOVEL     CCDCAT        MCATRA
118500961018     C                   MOVEL     CCDRES        MCDRES
118600130619     C*
118700961018     C                   MOVE      CMONTO        MVRTRA
118800050622     C                   Z-ADD     *ZEROS        WVRIVA           12 0
118900050622     C                   MOVE      CVRIVA        WVRIVA
119000130619     C*
119100050622     C     WVRIVA        DIV       100           WVRIVA
119200050622     C                   Z-ADD     WVRIVA        DVRIVA
119300040615     C*                                                              |
119400050622     C                   Z-ADD     *ZEROS        WVRIVA           12 0
119500050622     C                   MOVE      CVRIVA        WVRIVA
119600050622     C     WVRIVA        DIV       100           WVRIVA
119700050622     C                   Z-ADD     WVRIVA        MVRIVA
119800130619     C*
119900130619     C*    --->          VALOR DEL IMP. AL CONSUMO REFORMA TRIB. 2013
120000130619     C*
120100130619     C                   Z-ADD     *ZEROS        WVRIAC
120200130619     C                   MOVE      CVRDON        WVRIAC
120300130619     C     WVRIAC        DIV       100           WVRIAC
120400130619     C                   Z-ADD     WVRIAC        MVRDIS
120500130619     C                   Z-ADD     WVRIAC        DVALTR
120600130619     C*
120700050622     C                   Z-ADD     *ZEROS        WVRIBS           12 0
120800050622     C                   MOVE      CVRIBS        WVRIBS
120900050622     C     WVRIBS        DIV       100           WVRIBS
121000050622     C                   Z-ADD     WVRIBS        DVRIBS
121100050622     C                   Z-ADD     *ZEROS        WVRIBS           12 0
121200050622     C                   MOVE      CVRIBS        WVRIBS
121300050622     C     WVRIBS        DIV       100           WVRIBS
121400050622     C                   Z-ADD     WVRIBS        MVRIBS
121500130619     C*
121600130619     C                   Z-ADD     *ZEROS        WVRACI           12 0
121700050622     C                   MOVE      CVRACI        WVRACI
121800050622     C     WVRACI        DIV       100           WVRACI
121900050622     C                   Z-ADD     WVRACI        MVRACI
122000130619     C*
122100040615     C                   MOVE      CINAC2        MINAC2
122200040615     C                   Z-ADD     *ZEROS        MINCHB
122300040615     C                   Z-ADD     *ZEROS        DINCHB
122400130619     C*
122500040615     C     CCDTYP        IFEQ      '18'
122600040615     C                   MOVE      1             MINCHB
122700040615     C                   MOVE      1             DINCHB
122800040615     C                   END
122900130906     C*
123000130906     C*    --->          GUARDA REF. PARA REPLICAR HACIA EL SIC
123100130619     C*
123200040615     C                   MOVE      CTRAC1        MTRAC1
123300130906     C                   EVAL      %SUBST(MTRAC1:61:10) = CFL10B
123400130906     C                   EVAL      %SUBST(MTRAC1:71:10) = CFL10C
123500130906     C*
123600100316     C*    FID 8015  TARJETA PRIVADA "ASSENDA" GRABAR CUENTA EN TRACK1
123700100316     C     CCOMPE        IFEQ      '8015'
123800100316     C                   MOVEL     CTARJ1        MTRAC1
123900100316     C                   END
124000130906     C*
124100130529     C*    FID 8016  TARJETA PRIVADA LAPOLAR (BIN: 636307)
124200130529     C*
124300130529     C                   EVAL      WADQPOL = ' '
124400130529     C                   MOVE      *ON           ICHK20C
124500130529     C*
124600130529     C                   IF        CCOMPE = '8016'
124700130529     C                   MOVE      0082          MCDCMP
124800130529     C                   EXSR      CHKTA01
124900130529     C*
125000130529     C*                  RUTINA DE EXCEPCION COMERCIOS LAPOLAR
125100130529     C*
125200130529     C                   IF        CCOMPE = '8016'
125300130529     C                   EXSR      CHK20C
125400130529     C                   ENDIF
125500130529     C*
125600130529     C                   ENDIF
125700130529     C*                                                              |
125800961018     C                   MOVEL     CFIL15        REALPE          100
125900961018     C                   MOVEL     CFIL16        REALPE          100
126000961018     C                   MOVEL     CDATTI        MDATIM
126100961018     C                   MOVEL     CSTAMS        MSTAMS
126200961018     C                   MOVEL     CORIGE        MORIGE
126300961018     C                   MOVEL     CENTIM        MENTIM
126400961018     C                   MOVEL     CEXTIM        MEXTIM
126500961018     C                   MOVEL     CREENT        MCTAAF
126600161115 fc  **Depura y alinea
126700161115     C                   Eval      Ptxtori = %Subst(Ccitty:1:7)
126800161115     C                   Eval      Ptxtdep = *Blanks
126900161115     C                   CallP     Depura_Carac(Ptipo: Pcodig: Ptxtori:
127000161115     C                                          Ptxtdep: Pind)
127100161115     C                   Movel     Ptxtdep       MCITTY
127200161111     C
127300961018     C                   MOVEL     CGRCRT        MGRCRT
127400961018     C                   MOVEL     CIDCRT        MIDCRT
127500970414     C                   MOVEL     CCREDA        MIDCRT
127600961018     C                   MOVEL     CVECTO        MVECTO
127700970401     C     WCUOTA        IFLT      '00'
127800970401     C                   Z-ADD     *ZEROS        MCUOTA
127900970401     C                   ELSE
128000970828     C                   Z-ADD     *ZEROS        MCUOTA
128100970828     C                   MOVE      WCUOTA        MCUOTA
128200970401     C                   END
128300961018     C*                                                              !
128400961018     C* RUTINA ANTERIOR SPDH                                         !
128500961018     C*                                                              !
128600961018     C                   MOVEA     CPROPI        AR
128700961018     C                   Z-ADD     10            X                 2 0
128800961018     C                   Z-ADD     10            I                 2 0
128900961018     C     X             DOWGT     0
129000961018     C     AR(X)         IFNE      *BLANKS
129100961018     C                   MOVE      AR(X)         DAT(I)
129200961018     C                   SUB       1             I
129300961018     C                   SUB       1             X
129400961018     C                   ELSE
129500961018     C                   SUB       1             X
129600961018     C                   END
129700961018     C                   END
129800961018     C*                                                              |
129900961018     C                   MOVEA     DAT           MDATPR
130000970828     C     CPROPI        IFNE      *BLANKS
130100970828     C     CCREDA        IFEQ      '0901'
130200970828     C     CCREDA        OREQ      '9901'
130300970828     C                   MOVE      CPROPI        MDATPR
130400970828     C                   MOVE      MDATPR        MPROPI
130500970828     C     MPROPI        DIV       100           MPROPI
130600970828     C                   END
130700961018     C                   END
130800961018     C*                                                              |
130900961018     C     MVRTRA        DIV       100           MVRTRA
131000130619     C*
131100130619     C*    --->          RESTA EL VR. PROPINA AL VR. TOTAL TRANS.
131200130619     C*
131300130619     C                   IF        MPROPI > *ZEROS
131400130619     C                   EVAL      MVRTRA =  MVRTRA - MPROPI
131500130619     C                   ENDIF
131600130619     C*
131700961018     C                   MOVE      CAJUST        MVRAJU
131800961018     C     MVRAJU        DIV       100           MVRAJU
131900961018     C                   MOVE      CREFER        WCDRE6
132000961018     C                   MOVE      0             MACTIV
132100961018     C*                                                              |
132200961018     C*LONGITUD DE AUTORIZACION DE 6 POSICIONES                      |
132300961018     C*                                                              |
132400961018     C     WFILRE        IFEQ      '.'
132500961018     C                   MOVE      WCDRE3        MCDREF
132600961018     C                   ELSE
132700961018     C                   MOVE      WCDRE1        MCDREF
132800961018     C                   MOVEL     '  '          MCDREF
132900961018     C                   END
133000961018     C*                                                              |
133100961018     C                   MOVE      6             MLGREF
133200961018     C                   MOVEL     CCDAJU        MCDRAZ
133300961018     C                   MOVE      CCDFLG        MCDMOD
133400961018     C*                                                              |
133500961018     C* GRABA REGISTRO DE OPERACIONES                                |
133600961018     C*                                                              |
133700961018     C* CONTROL DE AUTORIZACIONES VRS CANJE                          |
133800961018     C                   MOVE      MNUTER        WNUTER
133900101006     C                   MOVE      WNUTE1        WNUMTE
134000980316     C                   MOVEL     WCLIPP        XCLIPP            3
134100101006     C*
134200980316     C     XCLIPP        IFEQ      'CLI'
134300980316     C     XCLIPP        OREQ      'ASY'
134400980316     C     XCLIPP        OREQ      'ZTN'
134500980316     C     XCLIPP        OREQ      'TRM'
134600961018     C                   Z-ADD     *ZEROS        MCOMAU
134700961018     C                   ELSE
134800130319     C*
134900130319     C*    --->          IN69 = TRANS. ADQ. X REDEBAN/NO DEBE ENTRAR CHKT21
135000130319     C*
135100130319AC   C                   IF        *IN69 = '0'
135200961018     C                   EXSR      CHKT21
135300130319AC   C                   ENDIF
135400961018     C                   END
135500961018     C*                                                              !
135600010612     C     CCREDA        IFNE      '0901'
135700010612     C     CCREDA        ANDNE     '9901'
135800190808      * Valida si el valor de la propina es mayor a 0
135900190808 004 C     MVRAJU        IFGT      0
136000190808      * Valida si el tipo de terminal tiene habilitado el servicio de propina.
136100190808 004 C     MTIPTE        CHAIN     RELTAB25                           60
136200190808 004 C     *IN60         IFEQ      *OFF
136300190808 004 C     MTIPNE        CHAIN     RELTAB26                           61
136400190808 004 C     *IN61         IFEQ      *OFF
136500190808 004 C     MVRTRA        SUB       MVRAJU        MVRTRA
136600190808 004 C                   Z-ADD     MVRAJU        MPROPI
136700190808 004 C                   Z-ADD     *ZEROS        MVRAJU
136800190808 004 C                   END
136900190808 004 C                   END
137000190808 004 C                   END
137100010612     C*PROPINAS PARA AEROLINEAS                                     ||
137200090923     C*Se agrego tipo 6 para atender requerimiento de la DMA        ||
137300010612     C     MTIPTE        IFEQ      8
137400010612     C     MTIPTE        OREQ      5
137500090923     C     MTIPTE        OREQ      6
137600010612     C     MVRAJU        IFGT      0
137700010612     C     MVRTRA        SUB       MVRAJU        MVRTRA
137800010612     C                   Z-ADD     MVRAJU        MPROPI
137900010612     C                   Z-ADD     *ZEROS        MVRAJU
138000010612     C                   END
138100010612     C                   END
138200010612     C                   END
138300961018     C*                                                              |
138400961018     C*PROPINAS CON T6 O T7                                          !
138500961018     C*                                                              |
138600961018     C     CCDMES        IFEQ      '0220'
138700961018     C     CCDCAP        ANDEQ     '3'
138800961018     C                   EXSR      NOGRAB
138900961018     C                   ELSE
139000961018     C     MPROPI        IFGT      0
139100961018     C     MVRTRA        ADD       MPROPI        MVRTRA
139200961018     C                   END
139300961018     C*                                                             !!
139400961018     C     *IN22         IFEQ      '0'
139500961218     C     WCORED        ANDNE     *BLANKS
139600961018     C     RREDID        IFEQ      901
139700961018     C                   MOVEL     '00099901'    MNUTER
139800961018     C                   MOVE      '  '          MNUTER
139900961018     C                   END
140000961217     C     RREDID        IFEQ      902
140100961217     C                   MOVEL     '00099902'    MNUTER
140200961217     C                   MOVE      '  '          MNUTER
140300961217     C                   END
140400961018     C     WESRED        IFEQ      '1'
140500961018     C                   MOVEL     '00099902'    MNUTER
140600961018     C                   MOVE      '  '          MNUTER
140700961018     C                   END
140800961018     C                   END
140900961018     C*                                                             !!
141000981005     C     *IN22         IFEQ      '1'
141100980930     C                   MOVEL     MIDCRT        XIDCRT            4
141200980930     C     XIDCRT        IFEQ      '0901'
141300981005     C                   MOVEL     '00099901'    MNUTER
141400981005     C                   MOVE      '  '          MNUTER
141500981005     C                   END
141600981005     C     XIDCRT        IFEQ      'REDE'
141700981005     C                   MOVEL     '00099902'    MNUTER
141800981005     C                   MOVE      '  '          MNUTER
141900981005     C                   END
142000981005     C                   END
142100961018     C*GRABA                                                        ||
142200961018     C*                                                             ||
142300961018     C     *IN25         IFEQ      '0'
142400091001     C*
142500091001     C*    --->  ACTIVA LA FUNCIONALIDAD DEL PRODUCTO EMV O CHIP
142600091001     C*    CVCASH        IFNE      '051'
142700091001     C*
142800060530     C     CTEREX        ANDNE     '00086149'
142900000608     C     CTEREX        ANDNE     '00086464'
143000010227     C     CTEREX        ANDNE     '00092074'
143100010227     C     CTEREX        ANDNE     '00002783'
143200010227     C     CTEREX        ANDNE     '00002784'
143300990709     C                   Z-ADD     MVRTRA        FVRTRA
143400100514     C*
143500010731     C     XBINCO        IFEQ      '571839'
143600010731     C     XBINCO        OREQ      '571840'
143700011212     C     XBINCO        OREQ      '780101'
143800011212     C     XBINCO        OREQ      '780102'
143900010731     C                   Z-ADD     DVRIVA        MVRAJU
144000010731     C                   END
144100120724     C*
144200010911     C     MCDCMP        IFEQ      901
144300010911     C     MCDCMP        OREQ      9901
144400010911     C                   MOVE      CAJUST        MVRAJU
144500010911     C     MVRAJU        DIV       100           MVRAJU
144600120724     C                   ENDIF
144700120724     C*
144800020226     C                   MOVE      CTIPNE        WTIPNE
144900020424     C* SI CINDPR='P' => MRESCO=9 ; SI CINDPR='L' =>MRESCO=8
145000120724     C*
145100020424     C     CINDPR        IFEQ      'P'
145200020424     C                   Z-ADD     9             MRESCO
145300020424     C                   ENDIF
145400120724     C*
145500020424     C     CINDPR        IFEQ      'L'
145600020424     C                   Z-ADD     8             MRESCO
145700020424     C                   ENDIF
145800120724     C*
145900030622     C                   EXSR      TOKOPE
146000001205     C                   WRITE     SWROPERA
146100120724     C*
146200180924     C* EXTRACCIÓN INFO. TOKENIZACION
146300180924 001 C                   EXSR      MOPERA
146400180924 001 C                   EVAL      wDSEXTRA = CREGEX
146500180924 001 C                   EVAL      wINDFIN = 'N'
146600180924 001 C                   CallP     GEN_TOKEN( wDSOPERA : wDSEXTRA : wINDFIN )
146700180924     C*
146800120724     C                   IF        MCDCMP = 901  OR
146900120724     C                             MCDCMP = 9901 OR
147000120724     C                             MCDCMP = 34
147100010521     C                   MOVE      CVRIVA        MVRIVA
147200010522     C     MVRIVA        DIV       100           MVRIVA
147300050331     C                   MOVEL     MCDTAR        XTAR06            6
147400120724     C*
147500120724     C                   IF        (MCDRES < 010  AND
147600120724     C                              %SUBST(MCDTAR:01:06) = '     3')  OR
147700120724     C                             (MCDRES < 010  AND
147800120724     C                              %SUBST(MCDTAR:01:04) = '   6')
147900120724     C*
148000010521     C                   WRITE     SWRIVART
148100120724     C                   ENDIF
148200120724     C                   ENDIF
148300120724     C*
148400001205     C                   WRITE     SWROWTER
148500120724     C*
148600120724     C                   IF        CCAM25  = '08'  OR
148700120724     C                             CCAM25  = '15'  OR
148800120724     C                             CCAM25  = '59'
148900001205     C                   EXSR      TOKENS
149000120724     C                   ENDIF
149100120724     C*
149200091001     C* IF *NE 051-EMV   END
149300961018     C*                                                            |||
149400961018     C* GRABA REGISTROS SIN DEPOSITAR ADQUIRIDOS POR OTRAS REDES   |||
149500961018     C* FERNANDO GUTIERREZ 96/10/11:SE EXCLUYE REDEBAN             |||
149600961018     C*                                                            |||
149700961018     C     *IN22         IFEQ      '1'
149800961018     C     CCOMPE        IFEQ      '0901'
149900961018     C     CCOMPE        OREQ      '9901'
150000961018     C     CCOMPE        OREQ      'REDE'
150100961018     C     CCREDA        OREQ      '9901'
150200961018     C     CCREDA        OREQ      '0901'
150300961018     C     CCREDA        OREQ      'REDE'
150400970808     C     MCDCMP        IFLE      90
150500961018     C     MCDRES        IFLT      010
150600961018     C*                                                        |||||||
150700961018     C     CCOMPE        IFNE      'REDE'
150800961018     C     CCREDA        IFNE      'REDE'
150900970808     C                   MOVE      CCORED        MCDEST
151000961018     C                   WRITE     SWRSINDP
151100970216     C                   MOVE      '1'           WNOGRA
151200961018     C                   Z-ADD     15            MCDIDE
151300961018     C                   WRITE     SWRBAS01
151400961018     C                   END
151500961018     C                   END
151600961018     C*                                                        |||||||
151700961018     C                   MOVE      '1'           *IN31
151800961018     C                   END
151900961018     C                   END
152000961018     C                   END
152100961018     C                   END
152200961018     C*                                                            |||
152300961018     C                   ELSE
152400030212     C                   MOVEL     CTARJE        XBINT             6
152500030212     C     XBINT         IFEQ      '400558'
152600030212     C                   WRITE     SWRTESTE
152700030212     C                   Z-ADD     04            MCDIDE
152800030212     C                   WRITE     SWRBAS01
152900030212     C                   END
153000961018     C                   END
153100961018     C*                                                             ||
153200961018     C*GRABA                                                        ||
153300961018     C     MPROPI        IFGT      0
153400961018     C     MVRTRA        SUB       MPROPI        MVRTRA
153500961018     C                   END
153600961018     C                   END
153700070808     C*                                                              |
153800070808     C                   MOVE      'OK'          WINDVR            2
153900160309     C*                                                              |
154000160309     C                   IF        MVRTRA < 110 AND
154100160309     C                             CCDTYP <> '18'
154200070808     C                   MOVE      'XX'          WINDVR
154300160309     C                   ENDIF
154400160309     C*
154500160525     C*    ---->  VP     SE COMPENSAN TRANS. A&A CON VALOR DE COMPRA < 110
154600160309     C*
154700160309     C                   IF        MVRTRA < 110 AND
154800160309     C                             *IN23  = *ON
154900160309     C                   MOVE      'OK'          WINDVR
155000160309     C                   ENDIF
155100160525     C*
155200160608     C*    ---->  VNP    A&A CON VALOR DE COMPRA = 0
155300160525     C*
155400160601     C                   IF        MVRTRA = 0   AND
155500160525     C                             DCAPTU = 4   AND
155600160525     C                             MTIPTE = 5
155700160525     C                   MOVE      'OK'          WINDVR
155800160525     C                   ENDIF
155900961018     C*                                                              |
156000081002     C*    CAPTURA DE DATOS (CEF0500A)                               |
156100961018     C*                                                              |
156200091001     C*    --->  ACTIVA LA FUNCIONALIDAD DEL PRODUCTO EMV O CHIP
156300091001     C*    CVCASH        IFNE      '051'
156400091001     C     CCOMPE        IFNE      '8001'
156500021121     C     CCOMPE        ANDNE     '8002'
156600040217     C     CCOMPE        ANDNE     '8006'
156700080808     C     CCOMPE        ANDNE     '8015'
156800130529     C**JC CCOMPE        ANDNE     '8016'
156900050429     C     CCOMPE        ANDNE     '8902'
157000050915     C     CCOMPE        ANDNE     '8107'
157100071206     C     CCOMPE        ANDNE     '8207'
157200051020     C     CCOMPE        ANDNE     '8108'
157300060526     C     CCOMPE        ANDNE     '8102'
157400090122     C     CCOMPE        ANDNE     '4110'
157500111129     C     CCOMPE        ANDNE     '5110'
157600100803     C     CCOMPE        ANDNE     '8069'
157700101216     C     CCOMPE        ANDNE     '8209'
157800110426     C     CCOMPE        ANDNE     '0047'
157900110910     C     CCOMPE        ANDNE     '5019'
158000120102     C     CCOMPE        ANDNE     '8077'
158100131114     C     CCOMPE        ANDNE     '8086'
158200130523     C     CCOMPE        ANDNE     '8088'
158300140707     C     CCOMPE        ANDNE     '8004'
158400130529     C     ICHK20C       ANDNE     *OFF
158500130529     C     WADQPOL       ANDNE     'N'
158600100514     C*
158700100514     C*--> TRANS. DAVIVIENDA MSG 0220 Y 0221 SOLO PARA MONITOREO PRM.(PRMD)
158800100514     C*
158900100514     C     CADQUI        ANDNE     'PRMD'
159000021021     C     CFIL09        ANDNE     'AX'
159100100514     C*
159200961018     C     CCDCAP        IFEQ      '1'
159300961018     C     CCDCAP        OREQ      '3'
159400961018     C     MCDRES        IFGE      000
159500961018     C     MCDRES        ANDLE     009
159600961018     C     XBINCO        IFNE      '400558'
159700070808     C     WINDVR        IFEQ      'OK'
159800070808     C*          MVRTRA    IFGE 110                        -----.|||||
159900961018     C*                                                         ||||||
160000961018     C*  SE GRABA TIPO DE MOVIMIENTO Y BIN FUENTE               ||||||
160100961018     C*                                                         ||||||
160200961018     C                   Z-ADD     06            DTIPMO
160300961018     C                   Z-ADD     900000        DBINFU
160400961018     C*                                                         ||||||
160500961018     C*  BUSCA BIN DESTINO                                      ||||||
160600961018     C*                                                         ||||||
160700961018     C                   MOVE      WBINCO        WBINBU
160800961018     C                   EXSR      CHKTB8
160900961018     C     *IN18         IFEQ      '1'
161000961018     C                   MOVE      450724        DBINDE
161100961018     C                   ELSE
161200961018     C                   Z-ADD     WCBCLA        DBINDE
161300961018     C                   END
161400020424     C*                                                        |||||||
161500970923     C     MCDCMP        IFEQ      73
161600970923     C                   MOVE      900003        DBINDE
161700970923     C                   END
161800020424     C* PARA PREMIOS CON PROMOCION ELECTRON                     ||||||
161900020424     C     CINDPR        IFEQ      'P'
162000020424     C                   MOVE      900083        DBINDE
162100020424     C                   END
162200020424     C*                                                         ||||||
162300981020     C                   MOVE      MHOTRA        DREGNU
162400970923     C*                                                         ||||||
162500961018     C*  SE GRABA FECHA DEL DIA DE PROCESO                      ||||||
162600961018     C*                                                         ||||||
162700980806     C                   MOVE      XFECAN        XFEPRO
162800961018     C                   MOVE      XFEPRO        BFEPRO
162900961018     C*                                                         ||||||
163000961018     C*  BUSQUEDA DEL TERMINAL                                  ||||||
163100961018     C*                                                         ||||||
163200961018     C                   MOVE      MNUTER        WNUTER
163300101006     C                   MOVE      WNUTE1        WNUMTE
163400961018     C*                                                         ||||||
163500961018     C     WCLIPP        IFEQ      'CLIP'
163600961018     C     WCDES1        ANDEQ     032375
163700101006     C                   MOVE      '70107'       WNUMTE
163800961018     C                   END
163900961018     C*                                                         ||||||
164000961018     C     WCLIPP        IFEQ      'CLIP'
164100961018     C     WCDES1        ANDEQ     032367
164200101006     C                   MOVE      '71507'       WNUMTE
164300961018     C                   END
164400961018     C*                                                         ||||||
164500961018     C     WCLIPP        IFEQ      'CLIP'
164600961018     C     WCDES1        ANDEQ     234559
164700101006     C                   MOVE      '80107'       WNUMTE
164800961018     C                   END
164900961018     C*                                                         ||||||
165000961018     C     WCLIPP        IFEQ      'CLIP'
165100961018     C     WCDES1        ANDEQ     934976
165200101006     C                   MOVE      '72007'       WNUMTE
165300961018     C                   END
165400961018     C*                                                         ||||||
165500130319     C*
165600130319     C*    --->          IN69 = TRANS. ADQ. X REDEBAN/NO DEBE ENTRAR CHKT21
165700130319     C*
165800130319AC   C                   IF        *IN69 = '0'
165900961018     C                   EXSR      CHKT21
166000130319AC   C                   ENDIF
166100961018     C*                                                         ||||||
166200961018     C     *IN22         IFEQ      '0'
166300961218     C     WCORED        ANDNE     *BLANKS
166400961018     C                   MOVE      RCUENT        WCTADE
166500961018     C                   MOVE      RCUENT        MCTADE
166600961018     C                   END
166700961018     C*                                                         ||||||
166800961018     C* CUENTAS CORRIENTES PARA AEROLINEAS                      ||||||
166900961018     C*                                                         ||||||
167000961018     C                   EXSR      CHKBAN
167100961018     C                   Z-ADD     WBINRE        DBINRE
167200961018     C*                                                         ||||||
167300961018     C                   Z-ADD     *ZEROS        DCODOF
167400961018     C*                                                         ||||||
167500961018     C*  UBICACION DE LA TARJETA  13   16   19                  ||||||
167600961018     C*                                                         ||||||
167700961018     C     WFILL1        IFEQ      *BLANKS
167800961018     C     WFILL2        ANDEQ     *BLANKS
167900961018     C                   MOVEL     '000000'      DTARJE
168000961018     C                   MOVE      WTARJ1        DTARJE
168100961018     C                   ELSE
168200961018     C     WFILL1        IFNE      *BLANKS
168300961018     C     WFILL2        ANDEQ     *BLANKS
168400961018     C                   MOVEL     '000'         DTARJE
168500961018     C                   MOVE      WTARJ2        DTARJE
168600961018     C                   ELSE
168700961018     C                   MOVE      MCDTAR        DTARJE
168800961018     C                   END
168900961018     C                   END
169000961018     C*                                                         ||||||
169100980915     C                   EXSR      DINTAR
169200980915     C*                                                         ||||||
169300961018     C                   MOVE      MSETRA        DNUMCO
169400961018     C                   MOVE      MFHTRA        WFECOM
169500961121     C*                    MOVE WCDES1    WCODES                ||||||
169600961121     C                   MOVE      MCDEST        WCODES
169700961121     C*                                                         !!!!!!
169800050510     C                   EXSR      CHKPRO
169900961121     C*                                                         !!!!!!
170000961018     C                   EXSR      CHKBAS
170100961018     C                   MOVE      WPORDT        DPORDT
170200961018     C                   MOVE      MCDREF        DCODAU
170300961018     C*                                                         ||||||
170400130619     C*                                                         ||||||
170500130619     C*
170600130619     C*    --->          ASIGNA VALORES DE C&C
170700150831     C*
170800150831     C*    --->          IND. DE TOKEN DE CB / EXCEPCION DVRIAC: N/A
170900150831     C*
171000150831     C                   EVAL      WINDCB = %SUBST(CHDRQC:01:09)
171100150831     C                   IF        WINDCB > *BLANKS
171200150831     C                   EVAL      DVRIVA = *ZEROS
171300150831     C                   EVAL      DVALTR = *ZEROS
171400150831     C                   ENDIF
171500150831     C*
171600130619     C                   EVAL      DVALCO = MVRTRA - (DVRIVA + DVALTR)
171700130619     C                   EVAL      DVALTO = MVRTRA
171800130619     C*
171900130619     C                   IF        DBINDE = 450724
172000130619     C                   MOVE      0100          WPNTJE
172100130619     C                   EVAL (H)  WVALDE = DVALTO * WPNTJE
172200130619     C                   ELSE
172300130619     C                   EVAL (H)  WVALDE = DVALCO * WPNTJE
172400130619     C                   ENDIF
172500130619     C                   EVAL      DVALDE = WVALDE
172600961018     C*                                                         ||||||
172700130619     C*    --->          TRANS. ELECTRON CON TARIFA TII (EXEPCION)
172800130619     C*
172900970826     C                   MOVE      *BLANKS       RTPPRO
173000130619     C*
173100961018     C                   MOVEL     DBINDE        WBININ            9
173200961018     C                   MOVE      '000'         WBININ
173300961018     C                   MOVE      WBININ        KBININ
173400961018     C*                                                         ||||||
173500970826     C     KBININ        CHAIN     SIR0052A                           75
173600961018     C*                                                         ||||||
173700061028     C     DBINDE        IFNE      450724
173800061028     C     RTPPRO        IFEQ      'E'
173900061028     C     XTPPRO        OREQ      'E'
174000061028     C     MCDCMP        OREQ      73
174100061028     C     DBINDE        OREQ      560011
174200061028     C     DBINDE        OREQ      560062
174300061028     C     DBINDE        OREQ      603344
174400100126     C*    FID 9073  TARJETA PRIVADA SERVIBANCA
174500061028     C     DBINDE        OREQ      603119
174600100126     C*
174700061028     C     WCODES        CHAIN     ESRDESCU                           95
174800100126     C                   MOVE      *IN95         ICHKDES
174900061028     C*                                                       ||||||||
175000061028     C     *IN95         IFEQ      '0'
175100061028     C*                                                      |||||||||
175200061028     C     MINDI1        IFEQ      2
175300061028     C                   Z-ADD     *ZEROS        DPORDT
175400061028     C                   Z-ADD     MPROD1        DVALDE
175500061028     C                   END
175600061028     C*                                                      |||||||||
175700071212     C* CALCULAR TI CASH BACK SIN VR DE COMPRA               |||||||||
175800071212     C*                                                      |||||||||
175900071212     C     MINDI1        IFEQ      2
176000071212     C     CCDTYP        ANDEQ     '18'
176100071212     C     DVALCO        ANDEQ     0
176200071212     C                   Z-ADD     *ZEROS        DVALDE
176300071212     C                   END
176400071212     C*                                                      |||||||||
176500061028     C     MINDI1        IFEQ      1
176600061028     C                   Z-ADD     MPROD1        WDECTO            3 0
176700061028     C     WDECTO        CHAIN     RERTAB06                           16
176800061028     C                   MOVE      RPNTJE        WPNTJE
176900061028     C     WPNTJE        MULT(H)   100           WPORDT
177000061028     C                   MOVE      WPORDT        DPORDT
177100061028     C*IVA                                                  ||||||||||
177200061028     C     DVALCO        MULT(H)   WPNTJE        WVALDE
177300061028     C                   Z-ADD     WVALDE        DVALDE
177400061028     C*                                                     ||||||||||
177500061028     C                   END
177600061028     C*                                                      |||||||||
177700061028     C                   END
177800061028     C*                                                       ||||||||
177900061028     C                   END
178000961018     C                   END
178100130619     C*
178200130619     C                   IF        MPROPI > *ZEROS
178300130619     C                   EVAL      DVALPR = MPROPI
178400130619     C                   EVAL      DVALTO = DVALCO + DVALPR + DVRIVA + DVALTR
178500130619     C*
178600130619     C                   IF        DBINDE = 450724
178700130619     C                   MOVE      0100          WPNTJE
178800130619     C                   EVAL (H)  WVALDE = DVALTO * WPNTJE
178900130619     C                   EVAL      DVALDE = WVALDE
179000130619     C                   ENDIF
179100130619     C*
179200130619     C                   IF        DBINDE <> 450724 AND
179300130619     C                             CCDTYP <> '18'
179400130619     C*
179500130619     C                   EVAL (H)  WVALDE = DVALCO * WPNTJE
179600130619     C                   EVAL      DVALDE = WVALDE
179700130619     C*
179800130619     C*    --->          IND 71 (*ON) = NO HAY DERECHO A PROPINA
179900130619     C*
180000130619     C                   IF        *IN71  = '1'
180100130619     C                   EVAL      WCOMIS = DVALCO + DVALPR
180200130619     C                   EVAL (H)  WVALDE = WCOMIS * WPNTJE
180300130619     C                   EVAL      DVALDE = WVALDE
180400130619     C                   ENDIF
180500130619     C*
180600130619     C*    --->          IND 73 (*ON) = ES UNA TRANS AEROLINEAS O CASH BACK
180700130619     C*
180800130619     C                   IF        *IN73  = '1'
180900130619     C                   EVAL (H)  WVALDE = DVALCO * WPNTJE
181000130619     C                   EVAL      DVALDE = WVALDE
181100130619     C                   ENDIF
181200961018     C*                                                       !!!!!!!!
181300130619     C                   ENDIF
181400130619     C*
181500130619     C                   ENDIF
181600130619     C*
181700130619     C                   EVAL      DVALIN = DVALTO - DVALDE
181800130619     C*
181900970401     C     WCUOTA        IFLT      '00'
182000970401     C                   Z-ADD     *ZEROS        DPLAZO
182100970401     C                   ELSE
182200970828     C                   Z-ADD     *ZEROS        DPLAZO
182300970825     C                   MOVE      WCUOTA        DPLAZO
182400970401     C                   END
182500970401     C*                                                         ||||||
182600961018     C                   Z-ADD     *ZEROS        DCODMO
182700961018     C                   Z-ADD     *ZEROS        DTRANS
182800961018     C     DCAPTU        IFEQ      0
182900961018     C                   Z-ADD     2             DCAPTU
183000961018     C                   END
183100980806     C                   MOVE      MFHCON        XFHCON
183200980806     C                   MOVE      XDIARE        DDIARE
183300980806     C                   MOVE      XMESRE        DMESRE
183400980806     C                   MOVE      XANORE        DANORE
183500980806     C                   MOVE      DDIACA        DDIADE
183600980806     C                   MOVE      DMESCA        DMESDE
183700980806     C                   MOVE      DANOCA        DANODE
183800961018     C                   MOVEL     CFETRA        DFETRA
183900961018     C                   MOVE      MCTADE        DCTADE
184000961018     C*                                                         !!!!!!
184100961018     C* EN CASO DE QUE LA PROPINA SEA MAYOR QUE LA COMPRA       !!!!!!
184200961018     C*                                                         !!!!!!
184300070808     C*          DVALPR    IFGT DVALCO                     ----.!!!!!!
184400070808     C*                    Z-ADDDVALCO    WVALOR 110           !!!!!!!
184500070808     C*                    Z-ADD*ZEROS    DVALCO               !!!!!!!
184600070808     C*                    Z-ADD*ZEROS    DVALTO               !!!!!!!
184700070808     C*                    Z-ADD*ZEROS    DVALDE               !!!!!!!
184800070808     C*                    Z-ADD*ZEROS    DVALIN               !!!!!!!
184900070808     C*          WVALOR    ADD  DVALPR    WVRCAL 110           !!!!!!!
185000070808     C*          WVRCAL    MULT 15        DVALPR    H          !!!!!!!
185100070808     C*          DVALPR    DIV  100       DVALPR    H          |||||||
185200070808     C*                    Z-ADDWVRCAL    DVALCO               |||||||
185300070808     C*          DVALCO    ADD  DVALPR    DVALTO               |||||||
185400070808     C*          DVALTO    ADD  DVRIVA    DVALTO
185500070808     C*          DVALCO    MULT WPNTJE    WVALDE    H          |||||||
185600070808     C*                    Z-ADDWVALDE    DVALDE               |||||||
185700961018     C*                                                        |||||||
185800070808     C*          DBINDE    IFEQ 450724                     ---.|||||||
185900070808     C*                    MOVE 0100      WPNTJE              ||||||||
186000070808     C*          DVALTO    MULT WPNTJE    WVALDE    H         ||||||||
186100070808     C*                    Z-ADDWVALDE    DVALDE              ||||||||
186200070808     C*                    END                             ---.|||||||
186300961018     C*                                                        |||||||
186400070808     C*          DVALTO    SUB  DVALDE    DVALIN               !!!!!!!
186500070808     C*                    END                             ----.!!!!!!
186600961018     C*                                                         !!!!!!
186700961018     C*                                                         !!!!!!
186800961018     C     *IN22         IFEQ      '0'
186900961218     C     WCORED        ANDNE     *BLANKS
187000961018     C     RREDID        IFEQ      901
187100101006     C                   MOVE      '99901'       WNUMTE
187200961018     C                   END
187300101006     C*                                                         !!!!!!
187400961018     C     RREDID        IFEQ      902
187500101006     C                   MOVE      '99902'       WNUMTE
187600961018     C                   END
187700101006     C*                                                         !!!!!!
187800961018     C     WESRED        IFEQ      '1'
187900101006     C                   MOVE      '99902'       WNUMTE
188000961018     C                   END
188100961018     C                   END
188200961018     C*                                                         !!!!!!
188300961018     C                   MOVEL     WNUMTE        DNADA3
188400030213     C*                                                         ||||||
188500030213     C                   MOVE      CTIPNE        DTIPNE
188600030214     C                   MOVEL     CHOTRA        DHHTRA
188700030213     C                   MOVE      CCAICD        DCAICD
188800030213     C                   MOVE      CSECUE        DTRAID
188900030213     C                   MOVEL     CTERMI        DCATID
189000030213     C                   MOVE      *BLANKS       DPOSDC
189100030213     C                   MOVE      *BLANKS       DCAIMU
189200030213     C                   MOVE      CPEMOD        DPEMOD
189300030213     C                   MOVE      CPTCAP        DPTCAP
189400030213     C                   MOVE      CPCCOD        DPCCOD
189500030213     C                   MOVE      *BLANKS       DPSEMD
189600030213     C*                                                         ||||||
189700001205     C                   MOVE      CCDCIU        DCDCIU
189800001205     C                   MOVE      CCDESP        DCDESP
189900030622     C                   MOVE      *ZEROS        DNRFAC
190000001205     C                   MOVE      CNRFAC        DNRFAC
190100001205     C                   MOVE      CNRFAX        DNRFAX
190200001205     C                   MOVE      CNIESP        DNIESP
190300001205     C                   MOVE      CUNESP        DUNESP
190400001205     C                   MOVE      CMONXX        DMONXX
190500001205     C                   MOVE      CVCFAC        DVCFAC
190600001205     C                   MOVE      CFGPAG        DFGPAG
190700001205     C                   MOVE      CINTER        DINTER
190800001205     C                   MOVE      CTELEC        DTELEC
190900001205     C                   MOVE      CCVV2X        DCVV2X
191000001205     C                   MOVE      CCEDUL        DCEDUL
191100030213     C                   MOVE      CFGVAL        DFGVAL
191200030213     C                   MOVE      CESTRE        DESTRE
191300030213     C                   MOVE      CCONRE        DCONRE
191400030213     C                   MOVE      CPOSTA        DPOSTA
191500030213     C                   MOVE      CFGCEL        DFGCEL
191600030213     C                   MOVE      CTPTCI        DTPTCI
191700030213     C                   MOVE      CINDAD        DINDAD
191800030213     C                   MOVE      CCVDPR        DCVDPR
191900030213     C                   MOVE      CCAMUS        DCAMUS
192000030213     C                   MOVE      CIDIDT        DIDIDT
192100030213     C                   MOVE      CTRSET        DTRSET
192200030213     C                   MOVE      CSERTA        DSERTA
192300030213     C                   MOVE      CSERCO        DSERCO
192400070808     C*                                                         !!!!!!
192500070808     C* CAMPOS TOKEN Q7                                         !!!!!!
192600070808     C*                                                         !!!!!!
192700061106     C                   MOVE      CTHDQ7        DTHDQ7
192800061106     C                   MOVE      CPRDES        DPRDES
192900061106     C                   MOVE      CDPCQ7        DDPCQ7
193000061106     C                   MOVE      CFLDQ7        DFLDQ7
193100061106     C                   MOVE      CUSRQ7        DUSRQ7
193200070808     C*
193300111129     C*    --->          CAMPOS TOKEN PAGO X PARTES
193400070808     C*
193500070809     C     CHDRQP        IFEQ      *BLANKS
193600070809     C                   MOVE      *ZEROS        DVCREP
193700070809     C                   MOVE      *ZEROS        DVIVAP
193800070809     C                   MOVE      *ZEROS        DVBIVP
193900070809     C                   MOVE      *ZEROS        DNCUOP
194000070809     C                   MOVE      *ZEROS        DFH1CP
194100070809     C                   END
194200070809     C*
194300070808     C                   MOVE      CHDRQP        DHDRQP
194400070808     C                   MOVE      CNCREP        DNCREP
194500070808     C                   MOVE      CNCEDP        DNCEDP
194600070808     C*
194700110223     C                   Z-ADD     *ZEROS        WVRWRK
194800070808     C                   MOVE      CVCREP        WVRWRK
194900070808     C     WVRWRK        DIV       100           WVRWRK
195000070808     C                   Z-ADD     WVRWRK        DVCREP
195100070808     C*
195200070808     C                   Z-ADD     *ZEROS        WVRWRK
195300070808     C                   MOVE      CVIVAP        WVRWRK
195400070808     C     WVRWRK        DIV       100           WVRWRK
195500070808     C                   Z-ADD     WVRWRK        DVIVAP
195600070808     C*
195700070808     C                   Z-ADD     *ZEROS        WVRWRK
195800070808     C                   MOVE      CVBIVP        WVRWRK
195900070808     C     WVRWRK        DIV       100           WVRWRK
196000070808     C                   Z-ADD     WVRWRK        DVBIVP
196100070808     C*
196200070808     C                   MOVE      CNCUOP        DNCUOP
196300070808     C                   MOVE      CFH1CP        DFH1CP
196400070808     C                   MOVE      CBLK2P        DBLK2P
196500070808     C*
196600111129     C*    --->          CAMPOS TOKEN CORRESP. NO BANCARIOS
196700070808     C*
196800071114     C                   MOVE      CHDRQC        DHDRQC
196900071114     C                   MOVE      CINDPC        DINDPC
197000071114     C                   MOVE      CINDPC        MINCNB
197100071114     C                   MOVE      CNTARC        DNTARC
197200071114     C                   MOVE      CNTARC        MNTARC
197300071114     C                   MOVE      CBLK9C        DBLK9C
197400071114     C                   MOVE      CBLK9C        MBLK9C
197500150831     C*
197600150831     C                   IF        CINDPC <> ' ' AND
197700150831     C                             CCDTYP  = '18'
197800150831     C                   EVAL      %SUBST(DNADA3:19:02) = 'CB'
197900150831     C*
198000150831     C                   EVAL      DPORDT  = *ZEROS
198100150831     C                   EVAL      DVRADQ  = *ZEROS
198200150831     C                   ENDIF
198300100126     C*
198400111129     C*    --->          RUTINA MARCA DE CAPTURA
198500100126     C*
198600100126     C                   EXSR      RTNCAP
198700111116     C*
198800111129     C*    --->          RUTINA TRANS. CONTACT LESS
198900111116     C*
199000120924     C                   IF        %SUBST(CTRAC1:14:02) = '01'
199100111116     C                   EXSR      RCLESS
199200111116     C                   ELSE
199300120924     C*
199400120924     C*    --->          RUTINA TRANS. COMPRA SIN TARJETA(AUT. DESDE CELULAR
199500120924     C*                  CASO 01: No TARJETA = CELULAR + DIG. CHK
199600120924     C*
199700120924     C                   IF        %SUBST(CTRAC1:14:02) = '02'
199800120924     C                   EXSR      RCOMST
199900120924     C                   ELSE
200000140925     C*
200100140925     C*    --->          RUTINA TRANS. BILLETERA ELECTRONICA (PERA)
200200140925     C*
200300140925     C                   IF        %SUBST(CTRAC1:14:02) = '06'
200400140925     C                   EXSR      RTPERA
200500140925     C                   ELSE
200600140925     C*
200700111129     C*    --->          RUTINA ESPECIAL COOP. TAXIS
200800110124     C*
200900110124     C                   IF        %SUBST(CTRAC1:01:15) > '000000000000000'
201000110124     C                   EXSR      RCOOPT
201100110124     C                   ENDIF
201200111116     C*
201300111116     C                   ENDIF
201400120924     C*
201500120924     C                   ENDIF
201600140925     C*
201700140925     C                   ENDIF
201800140724     C*
201900140724     C*    --->          RUTINA TRANS. AGREGADORES
202000140724     C*
202100180830     C                   IF        CUSRQ8 = 'A '
202200180829     C     MTIPTE        CHAIN(N)  REFTAB38AG
202300180829     C                   IF        %FOUND(REFTAB38AG)
202400140724     C                   EXSR      RTNAGR
202500140724     C                   ENDIF
202600180829     C                   ENDIF
202700160527     C*
202800160527     C*    --->          EXCEPCION TRANS AVAN/EFECT (CTPTRA 15) COLPATRIA
202900160527     C*
203000160527     C                   IF        %SUBST(CCOMPE:03:02) = '19' AND
203100160527     C                             %SUBST(DCTADE:01:02) = '19' AND
203200160527     C                             %INT(CCDRES) < 010 AND
203300160527     C                             CCDMES = '0210'  AND
203400160527     C                             CCDTYP = '15'
203500160527     C                   EVAL      %SUBST(DNADA3:19:02) = '62'
203600160527     C                   ENDIF
203700170411     C*
203800170411     C*    --->          ASIGNA MARCACION DE GESTOR DE RECAUDO
203900170411     C*
204000170412     C                   IF        CIDGRE = '17'   AND
204100170411     C                             CCDMES = '0210' AND
204200170411     C                             CCDTYP = '10'
204300170411     C                   EVAL      %SUBST(DNADA3:19:02) = 'GR'
204400170411     C*
204500170411     C                   ENDIF
204600140724     C*
204700111116     C*    --->          VALIDA SI EL TERMINAL CORRESP. AL COMERCIO
204800111116     C*
204900010904     C                   MOVE      '0'           *IN76
205000961018     C     *IN23         IFEQ      '0'
205100010904     C     *IN28         ANDEQ     '0'
205200101006     C*                                                         ||||||
205300101006     C     WNUMTE        IFLT      '99000'
205400030206     C                   MOVEL     CTARJE        CTAR01            1
205500961018     C                   MOVE      WNUMTE        TNUMTE
205600961018     C                   MOVE      DCODES        TCODES
205700070808     C*                                                         ||||||
205800961018     C     KEYVAL        CHAIN     RERCODTE                           76
205900070808     C*                                                         ||||||
206000961018     C     *IN76         IFEQ      '1'
206100961018     C     *IN31         IFEQ      '0'
206200991022     C     CCOMPE        ANDNE     '0023'
206300991022     C     CCOMPE        ANDNE     '9023'
206400991022     C     CCOMPE        ANDNE     'REDE'
206500991022     C     CCOMPE        ANDNE     '0901'
206600991022     C     CCOMPE        ANDNE     '9901'
206700991022     C     XCLIPP        ANDNE     'CLI'
206800991022     C     XCLIPP        ANDNE     'ASY'
206900991022     C     XCLIPP        ANDNE     'ZTN'
207000991022     C     XCLIPP        ANDNE     'TRM'
207100030206     C     CTAR01        ANDEQ     '4'
207200991022     C                   WRITE     SWRPOSER
207300961018     C                   Z-ADD     40            MCDIDE
207400961018     C                   WRITE     SWRBAS01
207500110301     C*
207600110301     C                   ENDIF
207700110301     C                   ENDIF
207800110301     C                   ENDIF
207900110301     C                   ENDIF
208000120314     C*
208100120314     C*    --->          EXCEPCION LIQ. BINES PREFERENC. - BOGOTA
208200120314     C*
208300120314     C                   IF        (MTPTRA = 14 OR CCDMES = '0420') AND
208400120314     C                              CCOMPE = '8111'
208500120314     C                   EXSR      CHKCTP
208600120314     C                   ENDIF
208700110301     C*
208800120314     C*    --->          EXCEPCION LIQ. BINES PREFERENC. - COLPATRIA
208900110301     C*
209000110301     C                   IF        (MTPTRA = 14 OR CCDMES = '0420') AND
209100110301     C                              CCOMPE = '8019'
209200110301     C                   EXSR      CHKCTP
209300110301     C                   ENDIF
209400110301     C*
209500120314     C*    --->          GRABACION O ELIMINACION DE TRANS. DEL CANJE
209600110301     C*
209700110301     C                   IF        MTPTRA  = 14     OR
209800110301     C                             CCDMES  = '0420'
209900110301     C*
210000131106     C                   EXSR      CHKFID
210100131106     C
210200131106     C                   IF        CCOMPE = '0051' AND
210300131106     C                             WTPPRO = 'RF'
210400131106     C
210500131106     C                   EXSR      CHKCTP
210600131106     C
210700131106     C                   EVAL      %SUBST(DNADA3:19:02) = WTPPRO
210800131106     C                   EVAL      DFRANQ = %SUBST(B_DSUBCD:01:02)
210900131106     C
211000131106     C                   ENDIF
211100131106     C*
211200110301     C                   IF        CINDPR <> 'P'
211300110301     C                   WRITE     SWRELIMI
211400110301     C                   EXSR      ELIMIN
211500110301     C                   ENDIF
211600110301     C*
211700110301     C                   ENDIF
211800961018     C*                                                         ||||||
211900110301     C*                  ******************                     ||||||
212000110301     C*                  EN CASO DE AJUSTES                     ||||||
212100110301     C*                  ******************                     ||||||
212200961018     C*SPDH                                                     ||||||
212300961018     C     MTPTRA        IFEQ      21
212400961018     C     MTIPTE        ANDEQ     6
212500090923     C     MTIPTE        ANDNE     6
212600961018     C     KEYAJU        CHAIN     SWRCAPT1                           85
212700961018     C     *IN85         IFEQ      '0'
212800961018     C                   UPDATE    SWRCAPT1
212900961018     C                   END
213000961018     C                   ELSE
213100961018     C*                                                        |||||||
213200961018     C     MTPTRA        IFEQ      21
213300090923     C*    MTIPTE        ANDNE     6
213400961018     C                   WRITE     SWRELIMI
213500961018     C*                                                       ||||||||
213600961018     C                   EXSR      ELIMIN
213700010402     C*IVA                 Z-ADDMVRAJU    DVALCO              ||||||||
213800010402     C     MVRAJU        SUB       DVRIVA        DVALCO
213900961018     C                   Z-ADD     MPROPI        DVALPR
214000961018     C     DVALCO        ADD       DVALPR        DVALTO
214100010402     C*IVA                                                    ||||||||
214200010402     C     DVALTO        ADD       DVRIVA        DVALTO
214300961018     C     DBINDE        IFEQ      450724
214400961018     C                   MOVE      0100          WPNTJE
214500961018     C                   END
214600961018     C     DVALTO        MULT(H)   WPNTJE        WVALDE
214700961018     C                   Z-ADD     WVALDE        DVALDE
214800961018     C     DVALTO        SUB       WVALDE        DVALIN
214900080903     C*
215000080903     C*    --->  GRABA EN EL CEF0500A
215100080903     C*
215200970216     C     WNOGRA        IFEQ      '0'
215300961018     C     *IN76         IFEQ      '0'
215400961018     C     *IN17         IFEQ      '0'
215500961228     C     CCREDA        IFEQ      '0902'
215600961228     C     CCREDA        OREQ      'REDE'
215700970717     C     CCREDA        OREQ      'VISA'
215800961228     C                   ADD       1             WWRK01
215900961228     C                   ELSE
216000961228     C****** JULIO 11 96   EXSR CALIVA                     |||||||||||
216100961228     C                   EXSR      CALDES
216200980806     C                   EXSR      SECUEN
216300120613     C*
216400991004     C     DVALTO        IFGT      0
216500000927     C     DVALIN        ANDGT     0
216600060530     C     CTEREX        ANDNE     '00086149'
216700000608     C     CTEREX        ANDNE     '00086464'
216800010227     C     CTEREX        ANDNE     '00092074'
216900010227     C     CTEREX        ANDNE     '00002783'
217000010227     C     CTEREX        ANDNE     '00002784'
217100180829     C*
217200180829     C*                  VALIDA SI TX. ES DE MIEMBRO PRINCIPAL
217300180829     C*
217400180829     C                   EXSR      VALMIP
217500180829     C*
217600980806     C                   WRITE     CER0500A
217700981105     C                   END
217800961018     C                   END
217900961018     C                   ELSE
218000980915     C                   EXSR      DINNER
218100961018     C                   WRITE     SWRREDES
218200961018     C                   END
218300961018     C                   END
218400961018     C                   END
218500961018     C*                                                       ||||||||
218600961018     C                   ELSE
218700961018     C*                                                       ||||||||
218800961018     C     MTPTRA        IFNE      14
218900961018     C     CCDMES        ANDNE     '0420'
219000961018     C     WNOGRA        IFEQ      '0'
219100961018     C     *IN76         IFEQ      '0'
219200961018     C     *IN17         IFEQ      '0'
219300961228     C     CCREDA        IFEQ      '0902'
219400961228     C     CCREDA        OREQ      'REDE'
219500970717     C     CCREDA        OREQ      'VISA'
219600961228     C                   ADD       1             WWRK01
219700961228     C                   ELSE
219800961228     C*****JULIO 96        EXSR CALIVA                     |||||||||||
219900961228     C                   EXSR      CALDES
220000980806     C                   EXSR      SECUEN
220100120613     C*
220200000608     C     DVALTO        IFGT      0
220300000927     C     DVALIN        ANDGT     0
220400060530     C     CTEREX        ANDNE     '00086149'
220500000608     C     CTEREX        ANDNE     '00086464'
220600010227     C     CTEREX        ANDNE     '00092074'
220700010227     C     CTEREX        ANDNE     '00002783'
220800010227     C     CTEREX        ANDNE     '00002784'
220900180829     C*
221000180829     C*                  VALIDA SI TX. ES DE MIEMBRO PRINCIPAL
221100180829     C*
221200180829     C                   EXSR      VALMIP
221300180829     C*
221400980806     C                   WRITE     CER0500A
221500981105     C                   END
221600961018     C                   END
221700961018     C                   ELSE
221800980915     C                   EXSR      DINNER
221900961018     C                   WRITE     SWRREDES
222000961018     C                   END
222100961018     C                   END
222200961018     C                   END
222300961018     C                   END
222400961018     C                   END
222500961018     C                   END
222600961018     C                   END
222700961018     C                   END
222800961018     C                   END
222900961018     C                   END
223000961018     C                   END
223100961018     C*                                                              |
223200961018     C*                                                              |
223300961018     C*                    **************                            |
223400961018     C*                    CLEAR A CAMPOS                            |
223500961018     C*                    **************                            |
223600961018     C*                                                              |
223700961018     C*                                                              |
223800961018     C                   MOVE      '0'           CCDCAP
223900961018     C*                                                              |
224000020424     C                   MOVE      *ZEROS        MRESCO
224100961018     C                   MOVE      *BLANKS       MDISPO
224200961018     C                   MOVE      *ZEROS        MCDREF
224300961018     C                   MOVE      *BLANKS       WCDEST
224400961018     C                   MOVE      *BLANKS       MCDEST
224500961018     C                   MOVE      *BLANKS       MNUTER
224600961018     C                   MOVE      *BLANKS       WNUTER
224700961018     C                   MOVE      *BLANKS       MCDORI
224800961018     C                   MOVE      *BLANKS       MSETRA
224900961018     C                   MOVE      *BLANKS       MCDTAR
225000961018     C                   MOVE      *BLANKS       MCTAAF
225100961018     C                   MOVE      *BLANKS       WCDTAR
225200961018     C                   MOVE      *BLANKS       WCDTA1
225300961018     C                   MOVE      *BLANKS       WCDTA2
225400961018     C                   MOVE      *BLANKS       XBINCO
225500970524     C                   MOVE      *BLANKS       WAER06
225600970524     C                   MOVE      *BLANKS       WAER08
225700961018     C                   MOVE      *BLANKS       WCODAE
225800961018     C                   MOVE      *BLANKS       CAGENC
225900961018     C                   MOVE      *BLANKS       WAGENC
226000970225     C                   MOVE      *BLANKS       WAGEUN
226100961018     C                   MOVE      *BLANKS       DNADA3
226200961018     C                   MOVE      *BLANKS       COWTER
226300961018     C                   MOVE      *BLANKS       MOWTER
226400961018     C*                                                              |
226500961018     C                   Z-ADD     *ZEROS        MTIPTE
226600961018     C                   Z-ADD     *ZEROS        MCDMSG
226700961018     C                   Z-ADD     *ZEROS        MFHTRA
226800961018     C                   Z-ADD     *ZEROS        MHOTRA
226900961018     C                   Z-ADD     *ZEROS        MFHCON
227000961018     C                   Z-ADD     *ZEROS        MTPTRA
227100961018     C                   Z-ADD     *ZEROS        MTPTHJ
227200961018     C                   Z-ADD     *ZEROS        MCATRA
227300961018     C                   Z-ADD     *ZEROS        MCDRES
227400961018     C                   Z-ADD     *ZEROS        MVRTRA
227500961018     C                   Z-ADD     *ZEROS        MPROPI
227600961018     C                   Z-ADD     *ZEROS        MVRAJU
227700961018     C                   Z-ADD     *ZEROS        MCDMOD
227800961018     C                   Z-ADD     *ZEROS        DVALPR
227900961018     C                   Z-ADD     *ZEROS        MCOMAU
228000961018     C                   Z-ADD     *ZEROS        DCAPTU
228100101006     C                   MOVE      *ZEROS        WNUMTE
228200961018     C                   Z-ADD     *ZEROS        DCODES
228300970401     C                   Z-ADD     *ZEROS        DPLAZO
228400970401     C                   Z-ADD     *ZEROS        MCUOTA
228500961018     C                   Z-ADD     *ZEROS        TCODES
228600101006     C                   MOVE      *ZEROS        TNUMTE
228700961018     C*                                                              |
228800961018     C                   Z-ADD     *ZEROS        MCDIDE
228900961018     C*                                                              |
229000961018     C                   MOVE      *BLANKS       CPROPI
229100961018     C                   MOVE      *BLANKS       CCODES
229200961018     C                   MOVE      *BLANKS       CCODE7
229300961018     C                   MOVE      *BLANKS       WCTADE
229400961018     C                   MOVE      *BLANKS       CTERMI
229500961018     C                   MOVEA     *BLANKS       AR
229600961018     C                   MOVEA     *BLANKS       DAT
229700001205     C*                                                              |
229800001205     C                   Z-ADD     0             DCDCIU
229900001205     C                   Z-ADD     0             DCDESP
230000030622     C                   MOVE      *ZEROS        DNRFAC
230100001205     C                   MOVE      *BLANKS       DNRFAX
230200001205     C                   Z-ADD     0             DNIGEN
230300001205     C                   Z-ADD     0             DNIESP
230400001205     C                   MOVE      *BLANKS       DUNESP
230500001205     C                   MOVE      *BLANKS       DMONXX
230600001205     C                   Z-ADD     *ZEROS        DVCFAC
230700001205     C                   MOVE      *BLANKS       DFGPAG
230800001205     C                   MOVE      *BLANKS       DINTER
230900001205     C                   Z-ADD     *ZEROS        DTELEC
231000001205     C                   Z-ADD     *ZEROS        DCVV2X
231100001205     C                   Z-ADD     *ZEROS        DCEDUL
231200001205     C                   MOVE      *BLANKS       DDUPLI
231300001205     C                   MOVE      *BLANKS       DFGVAL
231400001205     C                   MOVE      *BLANKS       DESTRE
231500001205     C                   MOVE      *BLANKS       DCONRE
231600001205     C                   MOVE      *BLANKS       DPOSTA
231700001205     C                   MOVE      *BLANKS       DFGCEL
231800001205     C                   MOVE      *BLANKS       DTPTCI
231900001205     C                   MOVE      *BLANKS       DINDAD
232000001205     C                   MOVE      *BLANKS       DCVDPR
232100001205     C                   MOVE      *BLANKS       DCAMUS
232200001205     C                   MOVE      *BLANKS       DIDIDT
232300001205     C                   MOVE      *BLANKS       DTRSET
232400001205     C                   MOVE      *BLANKS       DSERTA
232500001205     C                   MOVE      *BLANKS       DSERCO
232600050302     C                   MOVE      *BLANKS       DFRANQ
232700071205     C                   MOVE      *BLANKS       DINDPC
232800071205     C                   MOVE      *BLANKS       DNTARC
232900071205     C                   MOVE      *BLANKS       DHDRQC
233000071205     C                   MOVE      *BLANKS       DBLK9C
233100050302     C                   Z-ADD     *ZEROS        DPCADQ
233200050302     C                   Z-ADD     *ZEROS        DVRADQ
233300050303     C                   Z-ADD     *ZEROS        DVALDS
233400001205     C*                                                              |
233500001205     C                   Z-ADD     0             MCDCIU
233600001205     C                   Z-ADD     0             MCDESP
233700001205     C                   Z-ADD     0             MNRFAC
233800001205     C                   MOVE      *BLANKS       MNRFAX
233900001205     C                   Z-ADD     0             MNIGEN
234000001205     C                   Z-ADD     0             MNIESP
234100001205     C                   MOVE      *BLANKS       MUNESP
234200001205     C                   MOVE      *BLANKS       MMONXX
234300001205     C                   Z-ADD     *ZEROS        MVCFAC
234400001205     C                   MOVE      *BLANKS       MFGPAG
234500001205     C                   MOVE      *BLANKS       MINTER
234600001205     C                   Z-ADD     *ZEROS        MTELEC
234700001205     C                   Z-ADD     *ZEROS        MCVV2X
234800001205     C                   Z-ADD     *ZEROS        MCEDUL
234900001205     C                   MOVE      *BLANKS       MDUPLI
235000001205     C                   MOVE      *BLANKS       MFGVAL
235100001205     C                   MOVE      *BLANKS       MESTRE
235200001205     C                   MOVE      *BLANKS       MCONRE
235300001205     C                   MOVE      *BLANKS       MPOSTA
235400001205     C                   MOVE      *BLANKS       MFGCEL
235500001205     C                   MOVE      *BLANKS       MTPTCI
235600001205     C                   MOVE      *BLANKS       MINDAD
235700001205     C                   MOVE      *BLANKS       MCVDPR
235800001205     C                   MOVE      *BLANKS       MCAMUS
235900001205     C                   MOVE      *BLANKS       MIDIDT
236000001205     C                   MOVE      *BLANKS       MTRSET
236100001205     C                   MOVE      *BLANKS       MSERTA
236200001205     C                   MOVE      *BLANKS       MSERCO
236300071205     C                   MOVE      *BLANKS       MINCNB
236400071205     C                   MOVE      *BLANKS       MNTARC
236500071205     C                   MOVE      *BLANKS       MBLK9C
236600041202     C                   Z-ADD     0             WNRREG            9 0
236700001205     C*                                                              |
236800961018     C                   MOVE      '0'           *IN31
236900961018     C*                                                              |
237000961018     C                   END
237100181004 001 C                   ENDIF
237200180924      *
237300180924      * LIBERA PGM - PARA FINALIZAR:
237400180924 001 CLR                 EVAL      wDSOPERA = *BLANKS
237500180924 001 CLR                 EVAL      wDSEXTRA = *BLANKS
237600180924 001 CLR                 EVAL      wINDFIN = 'S'
237700180924 001 CLR                 CallP     GEN_TOKEN( wDSOPERA : wDSEXTRA : wINDFIN )
237800181004      *
237900181004 001 CLR                 EVAL      wTarjetatk = *Blanks
238000181004 001 CLR                 EVAL      wIndtoken  = *Blanks
238100181008 001 CLR                 EVAL      wFinpgm    = 'S'
238200181004 001 CLR                 CALLP     VAL_TOKEN(wTarjetatk: wIndtoken: wFinpgm)
238300961018     C*
238400961018     CLR                 DSPLY                   WTOTAL
238500961018     C*
238600961018     C*                ----------------------------
238700961018     C*                CHAIN INSTITUCION FINANCIERA
238800961018     C*                ----------------------------
238900961018     C*
239000961018     C     CHKTB1        BEGSR
239100961018     C     WCBCLA        CHAIN     RERTAB01                           78
239200961018     C     *IN78         IFEQ      '0'
239300000713     C                   Z-ADD     RBANCO        WBANCO            4 0
239400961018     C                   ELSE
239500000713     C                   Z-ADD     90            WBANCO            4 0
239600961018     C                   END
239700961018     C*
239800961018     C                   ENDSR
239900961018     C*
240000120627     C*
240100120627     C*    --->          CHAIN ENTIDADES FINANCIERAS
240200120627     C*
240300961018     C*
240400961018     C     CHKBIN        BEGSR
240500990721     C     XBINCO        CHAIN     PLR0101A                           80
240600120627     C*
240700990721     C     *IN80         IFEQ      '0'
240800120627     C*
240900991005     C                   Z-ADD     MBANCO        WBANCO            4 0
241000120627     C*
241100180829     C                   MOVEL     'TAB08'       D_DCDTAB
241200180829     C                   EVAL      D_DCDBIN = %DEC(XBINCO:6:0)
241300180829     C                   MOVEL     CCOMPE        D_DCDFID
241400180829     C                   MOVEL     *BLANKS       BIN               6
241500180829     C
241600180829     C     CCOMPE        IFNE      *BLANKS
241700180829     C     KEY100D       CHAIN(n)  SIF0100B04                         94
241800180829     C     *IN94         IFEQ      *OFF
241900180829     C                   EVAL      BIN = %CHAR(D_DCDBIN)
242000180829     C     XBINCO        IFEQ      BIN
242100180829     C     CCOMPE        ANDEQ     D_DCDFID
242200180829     C                   Z-ADD     D_DCDCMP      WBANCO
242300180829     C                   MOVE      *BLANKS       INDI80            1
242400180829     C                   EVAL      INDI80 = %SUBST(D_DSUBCD:01:01)
242500180829     C                   IF        INDI80 = '1'
242600180829     C                   MOVE      *ON           *IN80
242700180829     C                   ENDIF
242800180829     C                   ENDIF
242900180829     C                   ENDIF
243000180829     C                   ENDIF
243100120627     C*
243200990721     C                   ELSE
243300000713     C                   MOVE      '0090'        WBANCO            4 0
243400990721     C                   MOVE      *BLANKS       XCHEQU            4
243500990721     C                   MOVEL     XBINCO        XCHEQU
243600120627     C*
243700990721     C     XCHEQU        IFEQ      '3333'
243800000713     C                   Z-ADD     80            WBANCO
243900990721     C                   END
244000120627     C*
244100990721     C     XCHEQU        IFEQ      '9090'
244200000713     C                   Z-ADD     81            WBANCO
244300990721     C                   END
244400120627     C*
244500990721     C     CCOMPE        IFEQ      '9073'
244600000713     C                   Z-ADD     73            WBANCO
244700990721     C                   END
244800120627     C*
244900991013     C     CCOMPE        IFEQ      '0023'
245000000713     C                   Z-ADD     23            WBANCO
245100991013     C                   END
245200120627     C*
245300991013     C     CCOMPE        IFEQ      '9023'
245400000713     C                   Z-ADD     23            WBANCO
245500991013     C                   END
245600120627     C*
245700990721     C                   END
245800970519     C*
245900961018     C                   ENDSR
246000961018     C*
246100961018     C*                --------------------
246200961018     C*                CHAIN BANCO RECEPTOR
246300961018     C*                --------------------
246400961018     C*
246500961018     C     CHKBAN        BEGSR
246600961018     C     WCOMPE        CHAIN     RERBANCO                           81
246700961018     C                   MOVE      RCBCLA        WBINRE            6 0
246800961018     C                   ENDSR
246900130619     C*
247000130619     C*
247100130619     C*    --->          CHAIN MAESTRO DE cOMERCIOS
247200130619     C*
247300961018     C*
247400961018     C     CHKBAS        BEGSR
247500150831     C*
247600150831     C                   Z-ADD     *ZEROS        WPORDT
247700150831     C                   Z-ADD     *ZEROS        WPNTJE
247800150831     C                   Z-ADD     *ZEROS        WVALDE
247900150831     C*
248000150831     C     WCODES        CHAIN     ESRBASIC                           95
248100150831     C                   MOVE      *IN95         ICHKBAS
248200961018     C                   Z-ADD     MCODES        DCODES
248300961018     C*
248400961018     C     MDEDUC        IFEQ      2
248500961018     C                   MOVE      '1'           *IN71
248600961018     C*                                                              !
248700961018     C                   ELSE
248800961018     C*                                                              !
248900961018     C                   MOVE      '0'           *IN71
249000150831     C                   ENDIF
249100961018     C*
249200961018     C* AEROLINEAS
249300961018     C*
249400961018     C     MTIPNE        IFEQ      4511
249500961018     C                   MOVE      '1'           *IN73
249600961018     C                   ELSE
249700961018     C                   MOVE      '0'           *IN73
249800150831     C                   ENDIF
249900961018     C*
250000961018     C     *IN73         IFEQ      '0'
250100961018     C     MTIPNE        IFGE      3000
250200961018     C     MTIPNE        ANDLE     3299
250300961018     C                   MOVE      '1'           *IN73
250400961018     C                   ELSE
250500961018     C                   MOVE      '0'           *IN73
250600150831     C                   ENDIF
250700150831     C                   ENDIF
250800961018     C*
250900070808     C     CCDTYP        IFEQ      '18'
251000070808     C                   MOVE      '1'           *IN73
251100150831     C                   ENDIF
251200070808     C*
251300150831     C                   IF        ICHKBAS = *OFF
251400150831     C*
251500050510     C     XTPPRO        IFEQ      'C'
251600961018     C     MDECTO        CHAIN     RERTAB06                           16
251700150831     C                   ENDIF
251800150831     C*
251900050510     C     XTPPRO        IFEQ      'D'
252000050510     C     MDECDE        CHAIN     RERTAB06                           16
252100150831     C                   ENDIF
252200150831     C*
252300961018     C                   MOVE      RPNTJE        WPNTJE
252400961018     C     WPNTJE        MULT(H)   100           WPORDT
252500150831     C*
252600150831     C                   ENDIF
252700150831     C*
252800961018     C                   ENDSR
252900961018     C*
253000961018     C*              *************************************
253100961018     C*              CHAIN DE BINES NALES O INTERNACINALES
253200961018     C*              *************************************
253300961018     C*
253400961018     C     CHKTB8        BEGSR
253500080901     C*
253600990721     C     WBINBU        CHAIN     RERTAB08                           18
253700080901     C*
253800990721     C     *IN18         IFEQ      '0'
253900990721     C                   MOVE      '0'           *IN17
254000990721     C     RTIBIN        IFEQ      'V'
254100990721     C                   MOVE      '1'           *IN18
254200150831     C                   ENDIF
254300080901     C*
254400180829     C                   MOVEL     'TAB08'       D_DCDTAB
254500180829     C                   EVAL      D_DCDBIN = %DEC(WBINBU:6:0)
254600180829     C                   MOVEL     CCOMPE        D_DCDFID
254700180829     C                   MOVEL     *BLANKS       BIN
254800180829     C
254900180829     C     CCOMPE        IFNE      *BLANKS
255000180829     C     KEY100D       CHAIN(n)  SIF0100B04                         94
255100180829     C     *IN94         IFEQ      *OFF
255200180829     C                   EVAL      BIN = %CHAR(D_DCDBIN)
255300180829     C     WBINBU        IFEQ      BIN
255400180829     C     CCOMPE        ANDEQ     D_DCDFID
255500180829     C                   MOVE      *BLANKS       INDI18            1
255600180829     C                   MOVE      *BLANKS       INDI17            1
255700180829     C                   EVAL      INDI18 = %SUBST(D_DSUBCD:01:01)
255800180829     C                   EVAL      INDI17 = %SUBST(D_DSUBCD:03:01)
255900180829     C                   IF        INDI18 = '1'
256000180829     C                   MOVE      *ON           *IN18
256100180829     C                   ENDIF
256200180829     C                   IF        INDI17 = '1'
256300180829     C                   MOVE      *ON           *IN17
256400180829     C                   ENDIF
256500180829     C                   ENDIF
256600180829     C                   ENDIF
256700180829     C                   ENDIF
256800180829     C*BUSCA SOLO POR EL BIN
256900180829     C                   MOVE      'TAB08'       D_DCDTAB
257000180829     C                   MOVE      WBINCO        D_DCDBIN
257100180829     C                   MOVE      *BLANKS       D_DCDFID
257200180829     C     KEY100D       CHAIN(n)  SIF0100B04                         93
257300180829     C     *IN93         IFEQ      *OFF
257400180829     C                   EVAL      BIN = %CHAR(D_DCDBIN)
257500180829     C     WBINBU        IFEQ      BIN
257600180829     C                   MOVE      *BLANKS       INDI18            1
257700180829     C                   MOVE      *BLANKS       INDI17            1
257800180829     C                   EVAL      INDI18 = %SUBST(D_DSUBCD:01:01)
257900180829     C                   EVAL      INDI17 = %SUBST(D_DSUBCD:03:01)
258000180829     C                   IF        INDI18 = '1'
258100180829     C                   MOVE      '1'           *IN18
258200180829     C                   ENDIF
258300180829     C                   IF        INDI17 = '1'
258400180829     C                   MOVE      '1'           *IN17
258500180829     C                   ENDIF
258600180829     C                   ENDIF
258700180829     C                   ENDIF
258800080901     C*
258900990721     C                   ELSE
259000080901     C*
259100990721     C     CCOMPE        IFEQ      '0901'
259200990721     C     CCOMPE        OREQ      '9901'
259300990721     C     CCOMPE        OREQ      'REDE'
259400990721     C     CCOMPE        OREQ      '0902'
259500990721     C     CCOMPE        OREQ      '0903'
259600990721     C     CCOMPE        OREQ      '0034'
259700991013     C     CCOMPE        OREQ      '0023'
259800991013     C     CCOMPE        OREQ      '9023'
259900990721     C                   MOVE      '1'           *IN17
260000990721     C                   ELSE
260100990721     C                   MOVE      '0'           *IN17
260200990721     C                   END
260300990721     C                   END
260400961018     C                   ENDSR
260500961018     C*
260600961018     C*              *******************
260700961018     C*              CHAIN DE TERMINALES
260800961018     C*              *******************
260900961018     C*
261000961018     C     CHKT21        BEGSR
261100961018     C     WNUMTE        CHAIN     RERTAB21                           20
261200961018     C     *IN20         IFEQ      '0'
261300061101     C                   Z-ADD     MTIPTE        RTIPDN            2 0
261400000524     C     MTIPTE        IFEQ      8
261500090923     C     MTIPTE        OREQ      6
261600061101     C     WCDAER        IFEQ      00
261700061101     C     WCDAER        OREQ      99
261800000524     C                   MOVE      4             MTIPTE
261900061101     C                   END
262000000524     C                   END
262100961018     C*                                                              |
262200101006     C     WNUMTE        IFEQ      '52290'
262300041202     C                   EXSR      CHKAVI
262400041202     C                   END
262500041202     C*                                                              |
262600961018     C     MTIPTE        IFEQ      8
262700090923     C     MTIPTE        OREQ      6
262800961202     C                   Z-ADD     *ZEROS        WAECOD           10 0
262900961202     C                   MOVE      MCDEST        WAECOD           10 0
263000961202     C     WAECOD        CHAIN     ESRAEREO                           24
263100961018     C     *IN24         IFEQ      '0'
263200961018     C                   MOVEA     *BLANKS       AER
263300961018     C                   MOVEA     RCTAAE        AER
263400961018     C                   MOVE      AER(1)        WCTADE
263500961018     C                   MOVE      AER(1)        MCTADE
263600961202     C                   ELSE
263700961018     C                   MOVE      *BLANKS       WCTADE
263800961018     C                   MOVE      *BLANKS       MCTADE
263900961018     C                   END
264000961018     C*                                                             !!
264100961202     C                   ELSE
264200961018     C*                                                             !!
264300961018     C                   MOVE      MCTADE        WCTADE
264400160808     C*                                                             !!
264500160808     C*    ---->         VALIDA SI ES UN AVANCE FINANDINA
264600160808     C*
264700160808     C                   IF        CCDMES = '0210' AND
264800160808     C                             CCDTYP = '15'   AND
264900160808     C                             MTIPTE = 2
265000160808     C*
265100160808     C                   MOVE      'TAB07'       C_DCDTAB
265200160808     C                   MOVE      WBINCO        C_DCDBIN
265300160808     C     KEY100C       CHAIN     SIR0102B                           95
265400160808     C                   MOVE      *IN95         ICHK102
265500160808     C*
265600160808     C                   IF        ICHK102 = *OFF
265700160808     C                   MOVE      '1'           *IN17
265800160808     C                   ENDIF
265900160808     C*
266000160808     C                   ENDIF
266100961018     C*                                                             !!
266200961018     C                   END
266300961018     C*                                                              |
266400961018     C*                                                              |
266500961018     C*   MCOMAU   --->  CARACTERISTICA DEL TERMINAL                 !
266600961018     C*                                                              |
266700961018     C     MTIPTE        IFEQ      1
266800961018     C                   Z-ADD     1             MCOMAU
266900961018     C                   END
267000961018     C*                                                              !
267100961018     C     MTIPTE        IFEQ      2
267200980316     C     MTICOM        IFNE      50
267300961018     C                   Z-ADD     20            MCOMAU
267400961018     C                   ELSE
267500961018     C     MTICOM        IFEQ      50
267600961018     C                   Z-ADD     25            MCOMAU
267700961018     C                   END
267800961018     C                   END
267900961018     C                   END
268000961018     C*                                                              !
268100961018     C     MTIPTE        IFEQ      3
268200961018     C                   Z-ADD     3             MCOMAU
268300961018     C                   END
268400961018     C*                                                              !
268500961018     C     MTIPTE        IFEQ      4
268600980316     C     MTICOM        IFNE      50
268700961018     C                   Z-ADD     40            MCOMAU
268800961018     C                   ELSE
268900961018     C     MTICOM        IFEQ      50
269000961018     C                   Z-ADD     45            MCOMAU
269100961018     C                   END
269200961018     C                   END
269300961018     C                   END
269400961018     C*                                                              !
269500961018     C     MTIPTE        IFEQ      5
269600961018     C     MMARTE        IFEQ      102
269700961018     C                   Z-ADD     50            MCOMAU
269800961018     C                   ELSE
269900961018     C*CONVENIOS                                                   |||
270000961018     C     MMARTE        IFEQ      601
270100961018     C                   Z-ADD     55            MCOMAU
270200961018     C                   Z-ADD     4             DCAPTU
270300961018     C                   END
270400961018     C                   END
270500961018     C                   END
270600961018     C*                                                              !
270700961018     C     MTIPTE        IFEQ      6
270800961018     C     MTIPTE        OREQ      7
270900961018     C                   Z-ADD     7             MCOMAU
271000961018     C                   END
271100961018     C*                                                              !
271200961018     C     MTIPTE        IFEQ      8
271300090923     C     MTIPTE        OREQ      6
271400961018     C                   Z-ADD     8             MCOMAU
271500000313     C*CONVENIOS AEROLINEAS                                         ||
271600000313     C     MMARTE        IFEQ      601
271700000313     C     MPBXA1        ANDEQ     601
271800000620     C                   Z-ADD     60            MCOMAU
271900000913     C                   Z-ADD     4             DCAPTU
272000000313     C                   END
272100961018     C                   END
272200961018     C*                                                              !
272300961018     C     MTIPTE        IFEQ      9
272400961018     C                   Z-ADD     9             MCOMAU
272500961018     C                   END
272600961018     C*                                                              !
272700980320     C     MTIPTE        IFEQ      10
272800980320     C                   Z-ADD     10            MCOMAU
272900980320     C                   END
273000980320     C*                                                              !
273100000822     C     MTIPTE        IFEQ      11
273200101006     C     WNUMTE        OREQ      '52290'
273300000822     C                   Z-ADD     65            MCOMAU
273400030227     C                   Z-ADD     4             DCAPTU
273500000822     C                   END
273600000822     C*                                                              !
273700000822     C     MTIPTE        IFEQ      12
273800000822     C                   Z-ADD     70            MCOMAU
273900000822     C                   END
274000000822     C*                                                              !
274100040706     C     MTIPTE        IFEQ      13
274200040706     C                   Z-ADD     75            MCOMAU
274300040706     C                   END
274400061004     C*                                                              !
274500061004     C     MTIPTE        IFEQ      14
274600061004     C                   Z-ADD     80            MCOMAU
274700061004     C                   END
274800070328     C*
274900070328     C     MTIPTE        IFEQ      15
275000070328     C                   Z-ADD     85            MCOMAU
275100070328     C                   END
275200040706     C*                                                              !
275300980804     C     CCREDA        IFEQ      'VISA'
275400980804     C                   Z-ADD     40            MCOMAU
275500980804     C                   END
275600980804     C     CCREDA        IFEQ      '0901'
275700980804     C                   Z-ADD     40            MCOMAU
275800980804     C                   END
275900980804     C     CCREDA        IFEQ      '9901'
276000980804     C                   Z-ADD     40            MCOMAU
276100980804     C                   END
276200980804     C     CCREDA        IFEQ      '0902'
276300980804     C                   Z-ADD     40            MCOMAU
276400980804     C                   END
276500980804     C     CCREDA        IFEQ      'REDE'
276600980804     C                   Z-ADD     40            MCOMAU
276700980804     C                   END
276800030714     C     CCREDA        IFEQ      'AX03'
276900030714     C                   Z-ADD     40            MCOMAU
277000030714     C                   END
277100980804     C*                                                              |
277200961018     C                   ELSE
277300110124     C*
277400980804     C                   Z-ADD     40            MCOMAU
277500110124     C*
277600961018     C                   END
277700961018     C*
277800110124     C                   IF        CCAM25 = '15'
277900110124     C                   EXSR      CHKPFF
278000110124     C                   ENDIF
278100001205     C*
278200961018     C                   ENDSR
278300110124     C*
278400110124     C*                  ***************
278500110124     C*                  CALCULO DEL IVA
278600110124     C*                  ***************
278700961018     C*
278800961018     C     CALIVA        BEGSR
278900961018     C*
279000961018     C     DBINDE        IFNE      450724
279100961018     C     MCIUDA        IFNE      88001
279200961018     C     DVALDE        MULT(H)   .16           DVALIV           11 0
279300961018     C                   ADD       DVALIV        DVALDE
279400961018     C     DVALTO        SUB       DVALDE        DVALIN
279500961018     C                   END
279600961018     C                   END
279700961018     C*
279800961018     C                   ENDSR
279900961018     C*
280000961218     C*              *******************
280100961218     C*              CALCULO DEL 90%10%
280200961218     C*              *******************
280300961218     C     CALDES        BEGSR
280400961227     C*
280500961228     C                   Z-ADD     *ZEROS        X1                3 0
280600961228     C                   Z-ADD     *ZEROS        X2                6 0
280700961228     C                   MOVE      *BLANKS       XTARJE
280800961228     C*
280900961227     C                   MOVE      '0'           *IN44
281000961227     C                   MOVEL     DTARJE        X1                3 0
281100961227     C                   MOVEL     DTARJE        X2                6 0
281200961227     C     X2            IFEQ      0
281300961227     C                   MOVE      DTARJE        WTRECE           13 0
281400961227     C                   MOVEL     WTRECE        XTARJE           19
281500961227     C                   ELSE
281600961227     C     X1            IFEQ      0
281700961227     C                   MOVE      DTARJE        WSEISE           16 0
281800961228     C                   MOVEL     WSEISE        XTARJE
281900961227     C                   END
282000961227     C                   END
282100961218     C*
282200970425     C                   CALL      'SI0058'
282300961227     C     XTARJE        PARM                    XTARJE
282400961227     C     XCDBIN        PARM                    XCDBIN            6
282500961227     C     XLOTAR        PARM                    XLOTAR            2
282600961227     C     XINLIM        PARM                    XINLIM            1
282700961227     C     XINCHK        PARM                    XINCHK            1
282800961227     C     XTPPRO        PARM                    XTPPRO            1
282900961227     C*
283000050302     C     DBINDE        IFEQ      485935
283100030108     C     DBINDE        ANDNE     485930
283200021220     C     DBINDE        ANDNE     435651
283300021220     C     DBINDE        ANDNE     485926
283400030108     C     DBINDE        ANDNE     485958
283500021129     C     DBINDE        ANDNE     485924
283600021220     C     DBINDE        ANDNE     485936
283700030210     C     DBINDE        ANDNE     485929
283800030617     C     DBINDE        ANDNE     485970
283900030627     C     DBINDE        ANDNE     485946
284000030924     C     DBINDE        ANDNE     485980
284100031002     C     DBINDE        ANDNE     485979
284200040310     C     DBINDE        ANDNE     485991
284300040629     C     DBINDE        ANDNE     487048
284400040908     C     DBINDE        ANDNE     485953
284500041103     C     DBINDE        ANDNE     485995
284600041209     C     DBINDE        ANDNE     462947
284700070215     C     DBINDE        ANDNE     477364
284800070215     C     DBINDE        ANDNE     480851
284900070215     C     DBINDE        ANDNE     480852
285000961218     C*
285100050302     C                   Z-ADD     *ZEROS        DVALDE
285200961218     C*
285300961218     C     DVALTO        SUB       DVALDE        DVALIN
285400961218     C*
285500961218     C                   END
285600050310     C*
285700121121     C*    EXCEPCION LIQ. TARIFA TII BINES PRIVADOS : SERVIBANCA
285800121121     C*
285900121121     C                   IF        MCDCMP = 0073
286000121121     C*
286100050310     C                   MOVE      DPORDT        DPCADQ
286200050310     C                   Z-ADD     DVALDE        DVRADQ
286300121121     C*
286400050310     C     DVALTO        SUB       DVRADQ        DVALDS
286500121121     C*
286600060531     C                   Z-ADD     *ZEROS        DPORDT
286700060531     C                   Z-ADD     *ZEROS        DVALDE
286800121121     C*
286900060531     C     DVALTO        SUB       DVALDE        DVALIN
287000050310     C                   MOVE      'SB'          DFRANQ
287100121121     C                   ENDIF
287200121121     C*
287300121121     C*    EXCEPCION LIQ. TARIFA TII BIN   PRIVADO  : COOPCENTRAL
287400121121     C*
287500121121     C                   IF        MCDCMP = 0008   AND
287600121121     C                             DBINDE = 590708
287700121121     C*
287800121121     C                   MOVE      DPORDT        DPCADQ
287900121121     C                   Z-ADD     DVALDE        DVRADQ
288000121121     C*
288100121121     C     DVALTO        SUB       DVRADQ        DVALDS
288200121121     C*
288300121121     C                   Z-ADD     *ZEROS        DPORDT
288400121121     C                   Z-ADD     *ZEROS        DVALDE
288500121121     C*
288600121121     C     DVALTO        SUB       DVALDE        DVALIN
288700121121     C                   MOVE      '08'          DFRANQ
288800121121     C*
288900121121     C                   ENDIF
289000120314     C*
289100120314     C*                  EXCEPCION LIQ. BINES PREFERENC. - BOGOTA
289200120314     C*
289300120314     C                   IF        CCOMPE = '8111'
289400120314     C                   EXSR      CHKCTP
289500120314     C                   ENDIF
289600120314     C*
289700120314     C*                  FID 8111 TARJETA "CAFETERA"-(COMO ADQ:DCODOF = 77)
289800120314     C*
289900120314     C                   IF        CCOMPE = '8111'
290000120314     C*
290100120314     C                   EVAL      MCDCMP  = 0001
290200120314     C*
290300120314     C                   IF        ICHKDES = *OFF AND
290400120314     C                             %SUBST(DCTADE:01:02) = '01'
290500120314     C*
290600120314     C                   EVAL      DBINDE  = 639613
290700120314     C                   EVAL      DFRANQ  = 'BG'
290800120314     C*
290900120314     C                   EVAL      DPORDT  = *ZEROS
291000120314     C                   EVAL      DVALDE  = *ZEROS
291100120314     C*
291200120314     C                   EVAL      DVALIN  = DVALTO - DVALDE
291300120314     C                   EVAL      DVALDS  = DVALTO - DVRADQ
291400120314     C                   ENDIF
291500120314     C*
291600120314     C                   ENDIF
291700100126     C*
291800110301     C*                  RUTINA DE EXCEPCION LIQ. BINES PREFERENC.
291900110301     C*
292000110301     C                   IF        CCOMPE = '8019'
292100110301     C                   EXSR      CHKCTP
292200110301     C                   ENDIF
292300131106     C*
292400131106     C*                  EXCEPCION LIQ. BINES RETAIL FINANCIERO
292500131106     C*
292600131106     C                   EXSR      CHKFID
292700131106     C*
292800131106     C                   IF        CCOMPE = '0051' AND
292900131106     C                             WTPPRO = 'RF'
293000131106     C*
293100131106     C                   EXSR      CHKCTP
293200131106     C*
293300131106     C                   EVAL      %SUBST(DNADA3:19:02) = WTPPRO
293400131106     C                   EVAL      DFRANQ = %SUBST(B_DSUBCD:01:02)
293500131106     C*
293600131106     C                   ENDIF
293700130529     C*
293800131106     C*    --->          EXCEPCION LIQ. TII BIN PRIVADO: LA POLAR
293900130529     C*
294000130529     C                   IF        MCDCMP  = 0082   AND
294100130529     C                             DBINDE  = 636307 AND
294200130529     C                             ICHK20C = *ON    AND
294300130529     C                             WADQPOL = 'Y'
294400130529     C*
294500130529     C                   MOVE      DPORDT        DPCADQ
294600130529     C                   Z-ADD     DVALDE        DVRADQ
294700130529     C*
294800130529     C     DVALTO        SUB       DVRADQ        DVALDS
294900130529     C*
295000130529     C                   Z-ADD     *ZEROS        DPORDT
295100130529     C                   Z-ADD     *ZEROS        DVALDE
295200130529     C*
295300130529     C     DVALTO        SUB       DVALDE        DVALIN
295400130529     C                   MOVE      '82'          DFRANQ
295500130529     C*
295600130529     C                   ENDIF
295700110301     C*
295800100126     C                   ENDSR
295900100126     C*
296000120613     C*
296100120613     C*    --->          CALCULA NUMERO DE REFERENCIA
296200120613     C*
296300980806     C     SECUEN        BEGSR
296400980806     C*
296500980806     C                   MOVE      CSECUE        WSECUE            9
296600120613     C*
296700120613     C                   IF        WADQUI = '1'
296800980806     C                   MOVEL     '1'           WSECUE
296900120613     C                   ENDIF
297000120613     C*
297100120613     C                   IF        WADQUI = '2'
297200980806     C                   MOVEL     '2'           WSECUE
297300120613     C                   ENDIF
297400120613     C*
297500120613     C                   IF        MCOMAU = 65
297600030227     C                   MOVEL     '6'           WSECUE
297700030227     C                   MOVE      'A'           WADQUI
297800120613     C                   ENDIF
297900120613     C*
298000120613     C*          --->    ASIGNA (9) A TRANS. DE TERM. DESATENTIDAS
298100120613     C*
298200120613     C                   IF        MTIPTE = 17
298300120613     C                   MOVEL     '9'           WSECUE
298400120613     C                   ENDIF
298500120613     C*
298600980806     C                   MOVE      WSECUE        XSECUE
298700980806     C                   MOVE      WREFER        XREFER           21
298800120613     C*
298900980806     C                   MOVE      *BLANKS       XDIGRE            1
299000980806     C                   CALL      'BA0435'
299100980806     C                   PARM                    XREFER
299200980806     C                   PARM                    XDIGRE
299300980806     C                   MOVE      XDIGRE        DDIGRE
299400980806     C                   MOVEL     XSECUE        DBATRE
299500980806     C                   MOVE      XSECUE        DSECRE
299600980806     C*
299700980806     C     DBINDE        IFEQ      450724
299800980806     C                   Z-ADD     90            DCODOF
299900980806     C                   ELSE
300000980806     C                   MOVE      DBINDE        WBINDE            6
300100980924     C     WBINDE        CHAIN     PLR0101A                           11
300200980924     C     *IN11         IFEQ      '0'
300300980806     C                   Z-ADD     MBANCO        DCODOF
300400980806     C                   ELSE
300500980915     C     'ERROR'       DSPLY
300600980806     C                   END
300700980806     C                   END
300800980806     C*
300900980806     C                   ENDSR
301000961018     C*
301100961018     C*              ***********************************************
301200961018     C*              GRABACION  DE  OPERACION  DE  BATCH  UNICAMENTE
301300961018     C*              CUANDO SE TRATA DE UNA ANULACION DE TRANSACCION
301400961018     C*              ***********************************************
301500961018     C* OCTUBRE 26 VUELVE A GRABAR ANULACIONES DE REVERSIONES
301600961018     C*
301700961018     C     NOGRAB        BEGSR
301800961018     C     MTPTRA        IFEQ      21
301900961018     C     MTPTRA        OREQ      23
302000961018     C*          WNOGRA    IFEQ '0'                         ---------.
302100961018     C     CVCASH        IFNE      '051'
302200060530     C     CTEREX        ANDNE     '00086149'
302300010521     C     CTEREX        ANDNE     '00086464'
302400010521     C     CTEREX        ANDNE     '00092074'
302500010521     C     CTEREX        ANDNE     '00002783'
302600010521     C     CTEREX        ANDNE     '00002784'
302700010731     C     XBINCO        IFEQ      '571839'
302800010731     C     XBINCO        OREQ      '571840'
302900011212     C     XBINCO        OREQ      '780101'
303000011212     C     XBINCO        OREQ      '780102'
303100010731     C                   Z-ADD     DVRIVA        MVRAJU
303200010731     C                   END
303300120724     C*
303400010911     C     MCDCMP        IFEQ      901
303500010911     C     MCDCMP        OREQ      9901
303600010911     C                   MOVE      CAJUST        MVRAJU
303700010911     C     MVRAJU        DIV       100           MVRAJU
303800010911     C                   END
303900120724     C*
304000020226     C                   MOVE      CTIPNE        WTIPNE
304100030622     C                   EXSR      TOKOPE
304200120724     C*
304300010521     C                   WRITE     SWROPERA
304400120724     C*
304500180924 001 C* EXTRACCIÓN INFO. TOKENIZACION
304600180924 001 C                   EXSR      MOPERA
304700180924 001 C                   EVAL      wDSEXTRA = CREGEX
304800180924 001 C                   EVAL      wINDFIN = 'N'
304900180924     C                   CallP     GEN_TOKEN( wDSOPERA : wDSEXTRA : wINDFIN )
305000180924     C*
305100120724     C                   IF        MCDCMP  = 901  OR
305200120724     C                             MCDCMP  = 9901 OR
305300120724     C                             MCDCMP  = 34
305400010521     C                   MOVE      CVRIVA        MVRIVA
305500010522     C     MVRIVA        DIV       100           MVRIVA
305600120724     C*
305700120724     C                   IF        (MCDRES < 010  AND
305800120724     C                              %SUBST(MCDTAR:01:06) = '     3')  OR
305900120724     C                             (MCDRES < 010  AND
306000120724     C                              %SUBST(MCDTAR:01:04) = '   6')
306100120724     C                   WRITE     SWRIVART
306200120724     C                   ENDIF
306300120724     C                   ENDIF
306400120724     C*
306500961018     C                   WRITE     SWROWTER
306600120724     C*
306700120724     C                   ENDIF
306800120724     C*
306900961018     C*                    END                              ---------.
307000120724     C*
307100120724     C                   ENDIF
307200961018     C*
307300961018     C                   ENDSR
307400961018     C*
307500961018     C*                ----------------------------
307600961018     C*                ELIMINACION DE TRANSACCIONES
307700961018     C*                    DE CAPTURA DE DATOS
307800961018     C*                   DE TERMINALES HYPERCOM
307900961018     C*                ----------------------------
308000961018     C*
308100030731     C* LA ACCION DE 7LIMINAR TRANSACCIONES DE CAPTURA FUNCIONA ASI:
308200961018     C* BUSCA LA TRANSACCION EN EL ARCHIVO DE MOVIMIENTO DEL DIA DE
308300961018     C* DOS FORMAS 1 POR LLAVE TARJETA CODIGO Y VALOR
308400961018     C*            2 POR TARJETA Y CODIGO DE AUTORIZACION
308500961018     C* ASI NO SE ABONA LA TRANSACCION
308600961018     C* SI NO ENCUENTRA EL REGISTRO EN EL MOVIMIENTO DEL DIA SE GRABA U
308700961018     C* UN REGISTRO DE REVERSION EN EL ARCHIVO DE DEPOSITOS
308800961018     C*
308900961018     C     ELIMIN        BEGSR
309000961018     C                   MOVE      DTARJE        KTARJE
309100961018     C                   MOVE      DCODES        KCODES
309200961018     C                   Z-ADD     DNUMCO        KNUMCO
309300981119     C                   EXSR      CHDINC
309400961018     C                   MOVE      DCTADE        KCTADE
309500961018     C                   Z-ADD     DVALTO        KVALTO
309600980209     C     KEYEL1        CHAIN     SWRCAPT1                           82
309700961018     C     *IN82         IFEQ      '0'
309800980209     C                   DELETE    SWRCAPT1
309900961018     C                   ELSE
310000961018     C     KEYEL2        CHAIN     SWRCAPT2                           83
310100961018     C     *IN83         IFEQ      '0'
310200961018     C                   DELETE    SWRCAPT2
310300961018     C                   ELSE
310400980209     C     KEYELI        CHAIN     SWRCAPT3                           79
310500961018     C     *IN79         IFEQ      '0'
310600980209     C                   DELETE    SWRCAPT3
310700961018     C                   ELSE
310800030730     C                   MOVEL     CTARJE        B                 1
310900030730     C                   MOVE      'OK'          XREVER            2
311000030730     C     MCDCMP        IFEQ      23
311100030730     C     MCDCMP        OREQ      34
311200030730     C     MCDCMP        OREQ      50
311300030731     C     MCDCMP        OREQ      51
311400050302     C     MCDCMP        OREQ      01
311500050706     C     MCDCMP        OREQ      06
311600060403     C     MCDCMP        OREQ      32
311700061022     C     MCDCMP        OREQ      03
311800030730     C     B             IFNE      '4'
311900030730     C                   MOVE      'XX'          XREVER
312000030730     C                   END
312100030730     C                   END
312200030730     C     MCDRES        IFLT      10
312300030730     C     MCDCMP        IFLE      90
312400030730     C     XREVER        IFEQ      'OK'
312500030730     C     CCREDA        ANDNE     'REDE'
312600060311     C     MTPTRA        IFEQ      14
312700060311     C     MCDMSG        OREQ      420
312800060311     C                   WRITE     SWRREVER
312900060311     C                   Z-ADD     16            MCDIDE
313000060311     C                   WRITE     SWRBAS01
313100060311     C                   END
313200030730     C                   END
313300030730     C                   END
313400030730     C                   END
313500961018     C                   END
313600961018     C                   END
313700961018     C                   END
313800961018     C                   ENDSR
313900980915     C*         *********************************
314000980915     C*         LIQUIDACION TRANSACCIONES DINNERS
314100980915     C*         *********************************
314200980915     C     DINNER        BEGSR
314300091118     C*         20090723
314400091118     C*         SE AJUSTO PARA NO COMPENSAR TRNX ORIGINADAS EN
314500091118     C*         TERMINALES DIFERENTES A TIPO 8 MULTICOMERCIO Y
314600091118     C*         TERMINALES DIFERENTES A TIPO 6 (NEW TERMINAL)
314700091118     C*         AGENCIAS DE VIAJES/AEROLINEAS Y QUE NO SEAN DE
314800091118     C*         CADENALCO.SOLUCIONA EL REPORTE DE DAVIVIENDA
314900091118     C*         EN EL CUAL SE ESTAN COMPENSANDO TRNX DE PUNTO
315000091118     C*         PROPIO QUE NO CORRESPONDEN A CADENALCO
315100091118     C*         **********************************************
315200980915     C     CCOMPE        IFEQ      '0034'
315300980915     C                   MOVEL     DCODES        KCCOCV           16
315400030721     C*
315500030721     C                   MOVE      'SI'          WPROCE            2
315600091118     C*    RTIPDN        IFEQ      4
315700091118     C     RTIPDN        IFNE      8
315800091118     C     RTIPDN        ANDNE     6
315900091118     C     MCADEN        IFNE      935
316000091118     C*    MCADEN        ANDNE     935
316100030721     C                   MOVE      'NO'          WPROCE
316200030721     C                   END
316300091118     C                   END
316400980915     C     KCCOCV        CHAIN     ASRDINNE                           88
316500980915     C     *IN88         IFEQ      '0'
316600980915     C     MCMPDN        ANDNE     0
316700030721     C     WPROCE        ANDEQ     'SI'
316800980915     C*VALORES                                                      ||
316900980915     C*CARGO X TR.                                                  ||
317000980915     C     MILDNC        IFEQ      0
317100980915     C                   Z-ADD     *ZEROS        DPORDT
317200980915     C                   Z-ADD     MVLDNC        DVALDE
317300050302     C                   Z-ADD     *ZEROS        DPCADQ
317400050302     C                   Z-ADD     MVLDNC        DVRADQ
317500980915     C                   END
317600980915     C*PORCENTAJE                                                   ||
317700980915     C     MILDNC        IFEQ      1
317800980915     C                   Z-ADD     MVLDNC        DPORDT
317900050302     C                   Z-ADD     MVLDNC        DPCADQ
318000980915     C                   MOVE      MVLDNC        WPNTJE
318100010402     C*IVA                                                         |||
318200010402     C     DVALCO        MULT(H)   WPNTJE        WDINDE           11 0
318300980915     C                   Z-ADD     WDINDE        DVALDE
318400050302     C                   Z-ADD     WDINDE        DVRADQ
318500980915     C                   END
318600980915     C     DVALTO        SUB       DVALDE        DVALIN
318700050303     C     DVALTO        SUB       DVRADQ        DVALDS
318800980915     C*CUENTA                                                       ||
318900981119     C                   EXSR      DINCTA
319000981203     C                   Z-ADD     900006        DBINDE
319100980915     C                   EXSR      SECUEN
319200050330     C*                    EXSR CALICA                              ||
319300050302     C                   MOVE      'DN'          DFRANQ
319400980915     C                   WRITE     CER0500A
319500980915     C*                                                             ||
319600980915     C                   END
319700980915     C                   END
319800980915     C*
319900980915     C                   ENDSR
320000980915     C*         *************************
320100980915     C*         NUMERO DE TARJETA DINNERS
320200980915     C*         *************************
320300980915     C     DINTAR        BEGSR
320400980915     C*
320500980915     C     CCOMPE        IFEQ      '0034'
320600980915     C*TARJETA                                                       |
320700980915     C                   MOVEA     MCDTAR        NUM
320800980915     C                   Z-ADD     1             F                 2 0
320900980915     C     F             DOWLE     19
321000980915     C     NUM(F)        IFEQ      ' '
321100980915     C                   MOVE      '0'           NUM(F)
321200980915     C                   END
321300980915     C                   ADD       1             F
321400980915     C                   END
321500980915     C                   MOVEA     NUM           DTARJE
321600980915     C                   END
321700980915     C*
321800980915     C                   ENDSR
321900981119     C*         *************************
322000981119     C*         NUMERO DE CTA DEP DINNERS
322100981119     C*         *************************
322200981119     C     DINCTA        BEGSR
322300981119     C*
322400981119     C                   MOVEA     MCTADN        CTA
322500981119     C                   Z-ADD     19            I                 2 0
322600020111     C                   Z-ADD     19            Y                 2 0
322700981119     C                   MOVE      *ZEROS        CNU
322800981119     C     I             DOWGE     1
322900981119     C     CTA(I)        IFNE      ' '
323000981119     C                   MOVE      CTA(I)        CNU(Y)
323100981119     C                   SUB       1             I
323200981119     C                   SUB       1             Y
323300981119     C                   ELSE
323400981119     C                   SUB       1             I
323500981119     C                   END
323600981119     C                   END
323700020116     C                   MOVEA     CNU           XCTADE           19
323800020116     C                   MOVE      XCTADE        DCTADE
323900981203     C* BANCO ADQUIRENTE
324000981203     C                   MOVE      MCMPDN        WCMPDN            2 0
324100981203     C                   MOVEL     WCMPDN        DCTADE
324200981203     C     WCMPDN        CHAIN     RERBANCO                           77
324300981203     C                   Z-ADD     RCBCLA        DBINRE
324400981119     C*
324500981119     C                   ENDSR
324600981119     C*         ********************************
324700981119     C*         CHEQUE CUENTA PARA CANCELACIONES
324800981119     C*         ********************************
324900981119     C     CHDINC        BEGSR
325000981119     C*
325100981119     C                   MOVEL     DTARJE        XDINTA            6
325200981119     C     CCOMPE        IFEQ      '0034'
325300981119     C     XDINTA        ANDEQ     '000003'
325400981119     C                   MOVEL     DCODES        KCCOCV           16
325500981119     C     KCCOCV        CHAIN     ASRDINNE                           88
325600981119     C     *IN88         IFEQ      '0'
325700981119     C     MCMPDN        ANDNE     0
325800981119     C                   EXSR      DINCTA
325900981119     C                   END
326000981119     C                   END
326100981119     C*
326200981119     C                   ENDSR
326300981105     C*         ***************************************
326400981105     C*         PROCESO ELECTRONICO CRT PARA CELL POINT
326500981105     C*         ***************************************
326600981105     C     CELPOI        BEGSR
326700990625     C*
326800990625     C                   MOVEL     CTERMI        WCELPO            8
326900990625     C*CELLPOINT
327000990203     C     CAGENC        IFEQ      '10870962'
327100981204     C     WCELPO        IFEQ      'CLIP17ÑD'
327200981204     C     WCELPO        OREQ      'CLIP17#D'
327300981204     C*                                                             ||
327400990203     C                   MOVEL     '00072933'    CTERMI
327500981204     C                   MOVE      '1'           CCDCAP
327600981204     C*                                                             ||
327700981204     C                   END
327800981105     C                   END
327900990625     C*SOFTWARE EXPRESS
328000990625     C     CAGENC        IFEQ      '10915718'
328100990625     C     WCELPO        IFEQ      'CLIP17ÑB'
328200990625     C     WCELPO        OREQ      'CLIP17#B'
328300990625     C*                                                             ||
328400990625     C                   MOVEL     '00076783'    CTERMI
328500990625     C                   MOVE      '1'           CCDCAP
328600990625     C*                                                             ||
328700990625     C                   END
328800990625     C                   END
328900981105     C*
329000990216     C     CAGENC        IFEQ      '10153427'
329100990216     C                   MOVEL     '368001  '    CAGENC
329200990216     C                   END
329300990216     C     CAGENC        IFEQ      '10315174'
329400990216     C                   MOVEL     '403543  '    CAGENC
329500990216     C                   END
329600990216     C     CAGENC        IFEQ      '10092476'
329700990216     C                   MOVEL     '498428  '    CAGENC
329800990216     C                   END
329900990216     C*
330000981105     C                   ENDSR
330100111129     C*
330200111129     C*
330300111129     C*    --->          GUARDA INFORMACION DE LOS TOKENS
330400111129     C*
330500001205     C     TOKENS        BEGSR
330600001205     C*
330700001205     C                   MOVE      CCDCIU        MCDCIU
330800001205     C                   MOVE      CCDESP        MCDESP
330900001205     C                   MOVE      CNRFAC        MNRFAC
331000001205     C                   MOVE      CNRFAX        MNRFAX
331100001205     C                   MOVE      CNIESP        MNIESP
331200001205     C                   MOVE      CUNESP        MUNESP
331300001205     C                   MOVE      CMONXX        MMONXX
331400001205     C                   MOVE      CVCFAC        MVCFAC
331500001205     C                   MOVE      CFGPAG        MFGPAG
331600001205     C                   MOVE      CINTER        MINTER
331700001205     C                   MOVE      CTELEC        MTELEC
331800001205     C                   MOVE      CCVV2X        MCVV2X
331900001205     C                   MOVE      CCEDUL        MCEDUL
332000001205     C                   MOVE      CFGVAL        MFGVAL
332100001205     C                   MOVE      CESTRE        MESTRE
332200001205     C                   MOVE      CCONRE        MCONRE
332300001205     C                   MOVE      CPOSTA        MPOSTA
332400001205     C                   MOVE      CFGCEL        MFGCEL
332500001205     C                   MOVE      CTPTCI        MTPTCI
332600001205     C                   MOVE      CINDAD        MINDAD
332700001205     C                   MOVE      CCVDPR        MCVDPR
332800001205     C                   MOVE      CCAMUS        MCAMUS
332900001205     C                   MOVE      CIDIDT        MIDIDT
333000001205     C                   MOVE      CTRSET        MTRSET
333100001205     C                   MOVE      CSERTA        MSERTA
333200001205     C                   MOVE      CSERCO        MSERCO
333300001205     C*
333400001205     C                   WRITE     SWKOPHST
333500001205     C*
333600001205     C                   ENDSR
333700111129     C*
333800111129     C*
333900111129     C*    --->          GUARDA INFORMACION DE LOS TOKENS
334000111129     C*
334100030622     C     TOKOPE        BEGSR
334200030622     C*
334300030622     C                   MOVE      CCAICD        MCAICD
334400030622     C                   MOVE      *BLANKS       MCAIMU
334500030622     C                   MOVE      CPEMOD        MPEMOD
334600030622     C                   MOVE      CPTCAP        MPTCAP
334700030622     C                   MOVE      CPCCOD        MPCCOD
334800030622     C                   MOVE      *BLANKS       MPSEMD
334900030622     C                   MOVE      WCDAER        MCDAER
335000071205     C                   MOVE      CINDPC        MINCNB
335100071205     C                   MOVE      CNTARC        MNTARC
335200071205     C                   MOVE      CBLK9C        MBLK9C
335300030622     C*
335400030622     C                   ENDSR
335500111129     C*
335600030101     C*         ********************************
335700030101     C*                 CALCULA VALOR ICA
335800030101     C*         ********************************
335900050330     C*          CALICA    BEGSR
336000030101     C*
336100050330     C*          DCODES    CHAINESRBASIC             32
336200050330     C*                    MOVE MPRINC    WPRINC  65
336300050330     C*          DVALCO    MULT WPRINC    DVRICA    H
336400030101     C*
336500050330     C*                    ENDSR
336600041202     C*         ********************************
336700041202     C*                 AVIATUR
336800041202     C*         ********************************
336900111129     C*
337000041202     C     CHKAVI        BEGSR
337100041202     C*
337200041202     C     WNRREG        IFEQ      0
337300041202     C                   MOVE      MCDEST        KAEROL
337400041202     C                   Z-ADD     0020          KCIUDA
337500041202     C     KEYAE2        CHAIN     RERAVIAT                           68
337600041202     C     *IN68         IFEQ      '0'
337700041202     C                   Z-ADD     ACODIG        WCDAER
337800041202     C                   ELSE
337900041202     C     KAEROL        DSPLY
338000041202     C     KCIUDA        DSPLY
338100041202     C     'ERROR'       DSPLY
338200041202     C     'AVIATUR'     DSPLY
338300041202     C                   END
338400041202     C                   END
338500041202     C                   Z-ADD     8             MTIPTE
338600041202     C                   ADD       1             WNRREG
338700041202     C*
338800041202     C                   ENDSR
338900110124     C*
339000111129     C*    --->          TIPO PRODUCTO
339100110124     C*
339200050510     C     CHKPRO        BEGSR
339300050510     C*
339400050510     C                   Z-ADD     *ZEROS        X1                3 0
339500050510     C                   Z-ADD     *ZEROS        X2                6 0
339600050510     C                   MOVE      *BLANKS       XTARJE
339700061027     C                   MOVE      *BLANKS       XTPPRO
339800050510     C*
339900050510     C                   MOVE      '0'           *IN44
340000050510     C                   MOVEL     DTARJE        X1                3 0
340100050510     C                   MOVEL     DTARJE        X2                6 0
340200050510     C     X2            IFEQ      0
340300050510     C                   MOVE      DTARJE        WTRECE           13 0
340400050510     C                   MOVEL     WTRECE        XTARJE           19
340500050510     C                   ELSE
340600050510     C     X1            IFEQ      0
340700050510     C                   MOVE      DTARJE        WSEISE           16 0
340800050510     C                   MOVEL     WSEISE        XTARJE
340900050510     C                   END
341000050510     C                   END
341100050510     C*
341200050510     C                   CALL      'SI0058'
341300050510     C     XTARJE        PARM                    XTARJE
341400050510     C     XCDBIN        PARM                    XCDBIN            6
341500050510     C     XLOTAR        PARM                    XLOTAR            2
341600050510     C     XINLIM        PARM                    XINLIM            1
341700050510     C     XINCHK        PARM                    XINCHK            1
341800050510     C     XTPPRO        PARM                    XTPPRO            1
341900050510     C*
342000050510     C                   ENDSR
342100110124     C*
342200111116     C*    --->          CHAIN TABLA PFFBASIC - FACTURAS/RECAUDOS
342300110124     C*
342400110124     C     CHKPFF        BEGSR
342500110124     C*
342600110124     C                   MOVE      MCDEST        KESVIS
342700110124     C                   MOVE      MCDCMP        KCDVIS
342800110124     C*
342900110124     C     KEYPFF        CHAIN     PFRBASIC                           95
343000110124     C                   MOVE      *IN95         ICHKPFF
343100110124     C*
343200110124     C                   IF        ICHKPFF = *OFF
343300110124     C                   MOVE      MCTVIS        WCTADE
343400110124     C                   MOVE      MCTVIS        MCTADE
343500110124     C                   ELSE
343600110124     C                   MOVE      MCDEST        KESVIS
343700110124     C                   MOVE      0099          KCDVIS
343800110124     C*
343900110124     C     KEYPFF        CHAIN     PFRBASIC                           95
344000110124     C                   MOVE      *IN95         ICHKPFF
344100110124     C*
344200110124     C                   IF        ICHKPFF = *OFF
344300110124     C                   MOVE      MCTVIS        WCTADE
344400110124     C                   MOVE      MCTVIS        MCTADE
344500110124     C                   ELSE
344600110124     C                   MOVE      *ZEROS        WCTADE
344700110124     C                   MOVE      *ZEROS        MCTADE
344800110124     C                   ENDIF
344900110124     C*
345000110124     C                   ENDIF
345100110124     C*
345200110124     C                   ENDSR
345300100126     C*
345400111116     C*    --->          RUTINA MARCA DE CAPTURA
345500100126     C*
345600100106     C     RTNCAP        BEGSR
345700100106     C*
345800100106     C                   EVAL      DCAPTU  = 2
345900100106     C*
346000100106     C                   SELECT
346100100106     C*
346200100106     C                   WHEN      DFGCEL = '5'  OR
346300100106     C                             DFGCEL = '6'  OR
346400100106     C                             DFGCEL = '7'  OR
346500100106     C                             DFGCEL = '8'
346600100106     C                   EVAL      DCAPTU  = 4
346700100106     C*
346800100106     C                   WHEN      CPCCOD = '08' OR
346900100106     C                             CPCCOD = '59'
347000100106     C                   EVAL      DCAPTU  = 4
347100100106     C*
347200100106     C                   ENDSL
347300100106     C*
347400100106     C                   ENDSR
347500111116     C*
347600111116     C*    --->          RUTINA TRANS. CONTACT LESS
347700111116     C*
347800111116     C     RCLESS        BEGSR
347900111116     C*
348000111116     C                   EVAL      %SUBST(DINTER:01:15) = %SUBST(CTRAC1:01:15)
348100120924     C                   EVAL      %SUBST(DNADA3:19:02) = '01'
348200111116     C*
348300111116     C                   ENDSR
348400120924     C*
348500120924     C*    --->          RUTINA TRANS. COMPRA SIN TARJETA (AUT. DESDE CELULAR)
348600120924     C*
348700120924     C     RCOMST        BEGSR
348800120924     C*
348900120924     C                   EVAL      %SUBST(DINTER:01:15) = %SUBST(CTRAC1:01:15)
349000120924     C                   EVAL      %SUBST(DNADA3:19:02) = '02'
349100120924     C*
349200120924     C                   ENDSR
349300140925     C*
349400140925     C*    --->          RUTINA TRANS. BILLETERA ELECTRONICA (PERA)
349500140925     C*
349600140925     C     RTPERA        BEGSR
349700140925     C*
349800140925     C                   EVAL      %SUBST(DNADA3:19:02) = '06'
349900140925     C*
350000140925     C                   ENDSR
350100140925     C*
350200111116     C*    --->          RUTINA TRANS. COOP. TAXIS
350300110124     C*
350400110124     C     RCOOPT        BEGSR
350500110124     C*
350600110124     C                   EVAL      WSTARP = %SCAN('T': %SUBST(CTRAC1:01:15))
350700110124     C*
350800110124     C                   EXSR      CHKPFF
350900110124     C*
351000110124     C                   IF        ICHKPFF = *OFF
351100110124     C*
351200110124     C                   MOVE      'Y'           DFGPAG
351300110124     C                   MOVE      MCDBKP        DNIGEN
351400110124     C                   MOVE      MCDBKP        DNIESP
351500110124     C*
351600110124     C                   IF        WSTARP > *ZEROS
351700110124     C                   EVAL      WLNFLD = WSTARP - 1
351800110124     C                   EVAL      WSTARP = WSTARP - 1
351900110124     C                   EVAL      WSTARP = WENDPS - WSTARP
352000110124     C                   EVAL      DINTER = *ZEROS
352100110124     C                   EVAL      %SUBST(DINTER:WSTARP:WLNFLD) =
352200110124     C                             %SUBST(CTRAC1:01:WLNFLD)
352300110124     C                   ELSE
352400110124     C                   EVAL      %SUBST(DINTER:01:15) = %SUBST(CTRAC1:01:15)
352500110124     C                   ENDIF
352600110124     C*
352700110124     C                   ENDIF
352800110124     C*
352900110124     C                   ENDSR
353000110301     C*
353100111116     C*    --->          CHAIN A LA TABLA ESFCOMTP (T. PREFERENCIAL)
353200110301     C*
353300110301     C     CHKCTP        BEGSR
353400110301     C*
353500110301     C                   EVAL      WVRWRK  = *ZEROS
353600110301     C                   EVAL      WFLD02  = *ZEROS
353700110301     C*
353800110301     C                   MOVE      MCDCMP        I_MCDCMP
353900110301     C                   MOVE      MCODES        I_MCODES
354000110301     C                   EVAL      %SUBST(WFLD06:01:06) =
354100110301     C                             %SUBST(DTARJE:04:06)
354200110301     C                   MOVE      WFLD06        I_MCDBIN
354300110301     C*
354400110301     C     KEYCTP        CHAIN     ESRCOMTP                           95
354500110301     C                   MOVE      *IN95         ICHKCTP
354600110301     C*
354700110301     C                   SELECT
354800110301     C*
354900110301     C                   WHEN      ICHKCTP = *OFF
355000110301     C*
355100120314     C                   MOVE      I_MCDCMP      WCOMPE
355200120314     C                   EXSR      CHKBAN
355300120314     C                   Z-ADD     WBINRE        DBINRE
355400120314     C*
355500110301     C                   MOVE      I_MCDCMP      WFLD02
355600110307     C                   EVAL      DBINDE  = I_MCDBIN
355700110301     C                   EVAL      DCODOF  = I_MCDCMP
355800110301     C                   EVAL      DFRANQ  = WFLD02
355900110301     C*
356000110301     C                   EVAL      %SUBST(DCTADE:03:16) =
356100110301     C                             %SUBST(I_MCTADE:04:16)
356200110301     C                   EVAL      %SUBST(DCTADE:01:02) = WFLD02
356300110301     C*
356400111116     C*    --->          LIQ. VALOR FIJO
356500110301     C*
356600110301     C                   IF        I_MINLIQ = 0
356700110301     C*
356800110301     C                   EVAL      DPORDT = *ZEROS
356900110301     C                   EVAL      DVALDE = *ZEROS
357000110301     C                   EVAL      DPCADQ = *ZEROS
357100110301     C                   EVAL      DVRADQ = I_MVRLIQ
357200110301     C*
357300110301     C                   EVAL      DVALIN = DVALTO - DVALDE
357400110301     C                   EVAL      DVALDS = DVALTO - DVRADQ
357500110301     C*
357600110301     C                   END
357700110301     C*
357800111116     C*    --->          LIQ. VALOR PORCENTAJE
357900110301     C*
358000110301     C                   IF        I_MINLIQ = 1
358100110301     C*
358200110301     C                   EVAL      DPORDT = *ZEROS
358300110301     C                   EVAL      DVALDE = *ZEROS
358400110301     C                   EVAL      DPCADQ = I_MVRLIQ
358500110301     C*
358600110301     C                   IF        I_MVRLIQ > *ZEROS
358700110301     C                   EVAL      WPNTJE = I_MVRLIQ / 100
358800110301     C                   ELSE
358900110301     C                   EVAL      WPNTJE = I_MVRLIQ
359000110301     C                   ENDIF
359100110301     C*
359200110301     C                   EVAL (H)  WVRWRK = DVALCO * WPNTJE
359300110301     C                   EVAL      DVRADQ = WVRWRK
359400110301     C*
359500110301     C                   EVAL      DVALIN = DVALTO - DVALDE
359600110301     C                   EVAL      DVALDS = DVALTO - DVRADQ
359700110301     C*
359800110301     C                   END
359900110301     C*
360000110301     C                   OTHER
360100131106     C*
360200131106     C*    --->          TRANS. DE RETAIL QUE ADQ. <> A DAVIENDA
360300131106     C*
360400131106     C                   IF        WTPPRO = 'RF'
360500131106     C                   EVAL      WTPPRO = *BLANKS
360600131106     C                   ENDIF
360700110301     C*
360800130529     C                   ENDSL
360900130529     C*
361000130529     C                   ENDSR
361100130529     C*
361200130529     C*    --->          CHAIN ARCH. ESF1020C (COMERCIOS/EXCEPCION/LAPOLAR)
361300130529     C*
361400130529     C     CHK20C        BEGSR
361500130529     C*
361600130529     C                   EVAL      WVRWRK  = *ZEROS
361700130529     C                   EVAL      WFLD02  = *ZEROS
361800130529     C*
361900130529     C                   MOVE      *ON           ICHK20C
362000130529     C                   MOVE      MCDCMP        J_MCDCMP
362100130529     C                   MOVE      MCODES        J_MCODES
362200130529     C                   EVAL      %SUBST(WFLD06:01:06) =
362300130529     C                             %SUBST(MCDTAR:04:06)
362400130529     C                   MOVE      WFLD06        J_MCDBIN
362500130529     C*
362600130529     C     KEY20C        CHAIN     ESR1020C                           95
362700130529     C                   MOVE      *IN95         ICHK20C
362800130529     C*
362900130529     C                   ENDSR
363000140724     C*
363100140724     C*    --->          RUTINA TRANS. AGREGADORES
363200140724     C*
363300140724     C     RTNAGR        BEGSR
363400140724     C*
363500140724     C                   EVAL      %SUBST(DNADA3:19:02) = 'AN'
363600140724     C*
363700140724     C                   IF        CPEMOD = '07'
363800140724     C                   EVAL      %SUBST(DNADA3:19:02) = 'AK'
363900140724     C                   ENDIF
364000140724     C*
364100140728     C                   IF        %SUBST(CTRAC1:14:02) = '02' OR
364200140728     C                             %SUBST(CTRAC1:14:02) = '06'
364300140724     C                   EVAL      %SUBST(DNADA3:19:02) = 'AM'
364400140724     C                   ENDIF
364500140724     C*
364600140724     C                   ENDSR
364700140724     C*
364800130529     C*    --->            VALIDA SI LA ENTIDAD ESTA HABILITADA PARA RECIBIR
364900130529     C*                    TX DE LA POLAR
365000130529     C*
365100130529     C     CHKTA01       BEGSR
365200131106     C*
365300130529     C     WCOMPE        CHAIN     RERBANCO                           81
365400131106     C*
365500130529     C                   IF        *IN81 = *OFF
365600130529     C                   EVAL      WADQPOL = %SUBST(RPRINT:04:01)
365700130529     C                   ENDIF
365800131106     C*
365900130529     C                   ENDSR
366000131106     C*
366100131106     C*    --->          VALIDA FIID / COD. FRANQ. / TIPO PROD
366200131106     C*
366300131106     C     CHKFID        BEGSR
366400131106     C*
366500131106     C                   MOVE      CCOMPE        F_DCDFID
366600131106     C*
366700131106     C     KEYFID        CHAIN     SIR0100F                           95
366800131106     C                   MOVE      *IN95         ICHKFID
366900131106     C*
367000131106     C                   IF        ICHKFID  = *OFF
367100131106     C*
367200131106     C                   EVAL      B_DCDTAB = F_DTAB04
367300131106     C                   EVAL      B_DCDCMP = F_DCDCMP
367400131106     C                   EVAL      B_DCDBIN = DBINDE
367500131106     C*
367600131106     C     KEY100        CHAIN     SIR0100B                           95
367700131106     C                   MOVE      *IN95         ICHK100
367800131106     C*
367900131106     C                   IF        ICHK100  = *OFF
368000131106     C                   EVAL      WTPPRO   = %SUBST(B_DSUBCD:07:02)
368100131106     C                   ELSE
368200131106     C                   EVAL      WTPPRO   = *BLANKS
368300131106     C                   ENDIF
368400131106     C*
368500131106     C                   ENDIF
368600131106     C*
368700131106     C                   ENDSR
368800131106     C*
368900180829     C*    --->          VALIDA SI TX PERTENECE A MIEMBROS PRINCIPALES
369000180829     C*
369100180829     C     VALMIP        BEGSR
369200180829     C*
369300180829     C                   IF        DBINDE = 450724
369400180829     C     DBINRE        CHAIN     BAFTAB40
369500180829     C                   IF        %FOUND(BAFTAB40)
369600180829     C                   EVAL      DBINDE = 424994
369700180829     C                   ENDIF
369800180829     C                   ENDIF
369900180829     C                   ENDSR
370000180829     C*
370100180924     C*    --->  MUEVE EL REGISTRO DE SWFOPERA A LA DS PARA TOKENIZACION
370200180924     C*
370300180924 001 C     MOPERA        BEGSR
370400180924     C*
370500180924 001 C                   EVAL      O_MDISPO  =  MDISPO
370600180924 001 C                   EVAL      O_MCDCMP  =  MCDCMP
370700180924 001 C                   EVAL      O_MCDTAR  =  MCDTAR
370800180924 001 C                   EVAL      O_MCDMBR  =  MCDMBR
370900180924 001 C                   EVAL      O_MCDEST  =  MCDEST
371000180924 001 C                   EVAL      O_MNUTER  =  MNUTER
371100180924 001 C                   EVAL      O_MCDMSG  =  MCDMSG
371200180924 001 C                   EVAL      O_MCDORI  =  MCDORI
371300180924 001 C                   EVAL      O_MFHTRA  =  MFHTRA
371400180924 001 C                   EVAL      O_MHOTRA  =  MHOTRA
371500180924 001 C                   EVAL      O_MFHCON  =  MFHCON
371600180924 001 C                   EVAL      O_MSETRA  =  MSETRA
371700180924 001 C                   EVAL      O_MTPTRA  =  MTPTRA
371800180924 001 C                   EVAL      O_MTPTHJ  =  MTPTHJ
371900180924 001 C                   EVAL      O_MTPCTA  =  MTPCTA
372000180924 001 C                   EVAL      O_MCATRA  =  MCATRA
372100180924 001 C                   EVAL      O_MCDRES  =  MCDRES
372200180924 001 C                   EVAL      O_MVRTRA  =  MVRTRA
372300180924 001 C                   EVAL      O_MVRAJU  =  MVRAJU
372400180924 001 C                   EVAL      O_MCDREF  =  MCDREF
372500180924 001 C                   EVAL      O_MLGREF  =  MLGREF
372600180924 001 C                   EVAL      O_MCDRAZ  =  MCDRAZ
372700180924 001 C                   EVAL      O_MCDMOD  =  MCDMOD
372800180924 001 C                   EVAL      O_MACTIV  =  MACTIV
372900180924 001 C                   EVAL      O_MSTAMS  =  MSTAMS
373000180924 001 C                   EVAL      O_MORIGE  =  MORIGE
373100180924 001 C                   EVAL      O_MDATIM  =  MDATIM
373200180924 001 C                   EVAL      O_MENTIM  =  MENTIM
373300180924 001 C                   EVAL      O_MEXTIM  =  MEXTIM
373400180924 001 C                   EVAL      O_MCTAAF  =  MCTAAF
373500180924 001 C                   EVAL      O_MCITTY  =  MCITTY
373600180924 001 C                   EVAL      O_MGRCRT  =  MGRCRT
373700180924 001 C                   EVAL      O_MIDCRT  =  MIDCRT
373800180924 001 C                   EVAL      O_MVECTO  =  MVECTO
373900180924 001 C                   EVAL      O_MCLAUT  =  MCLAUT
374000180924 001 C                   EVAL      O_MCOMAU  =  MCOMAU
374100180924 001 C                   EVAL      O_MRESCO  =  MRESCO
374200180924 001 C                   EVAL      O_MTIPOR  =  MTIPOR
374300180924 001 C                   EVAL      O_MDISAC  =  MDISAC
374400180924 001 C                   EVAL      O_MMCNAL  =  MMCNAL
374500180924 001 C                   EVAL      O_MMCINT  =  MMCINT
374600180924 001 C                   EVAL      O_MTPTAR  =  MTPTAR
374700180924 001 C                   EVAL      O_MCLTAR  =  MCLTAR
374800180924 001 C                   EVAL      O_MCBTAR  =  MCBTAR
374900180924 001 C                   EVAL      O_MAUTOR  =  MAUTOR
375000180924 001 C                   EVAL      O_MCUOTA  =  MCUOTA
375100180924 001 C                   EVAL      O_MENCAD  =  MENCAD
375200180924 001 C                   EVAL      O_MCAICD  =  MCAICD
375300180924 001 C                   EVAL      O_MPOSDC  =  MPOSDC
375400180924 001 C                   EVAL      O_MCAIMU  =  MCAIMU
375500180924 001 C                   EVAL      O_MPEMOD  =  MPEMOD
375600180924 001 C                   EVAL      O_MPTCAP  =  MPTCAP
375700180924 001 C                   EVAL      O_MPCCOD  =  MPCCOD
375800180924 001 C                   EVAL      O_MPSEMD  =  MPSEMD
375900180924 001 C                   EVAL      O_MCDAER  =  MCDAER
376000180924 001 C                   EVAL      O_MFILER  =  MFILER
376100180924 001 C                   EVAL      O_MFILE1  =  MFILE1
376200180924 001 C                   EVAL      O_MFILE2  =  MFILE2
376300180924 001 C                   EVAL      O_MVRIVA  =  MVRIVA
376400180924 001 C                   EVAL      O_MVALTR  =  MVALTR
376500180924 001 C                   EVAL      O_MVRIBS  =  MVRIBS
376600180924 001 C                   EVAL      O_MINCHB  =  MINCHB
376700180924 001 C                   EVAL      O_MVRDIS  =  MVRDIS
376800180924 001 C                   EVAL      O_MVRACI  =  MVRACI
376900180924 001 C                   EVAL      O_MINAC1  =  MINAC1
377000180924 001 C                   EVAL      O_MINAC2  =  MINAC2
377100180924 001 C                   EVAL      O_MTRAC1  =  MTRAC1
377200180924 001 C                   EVAL      O_MINCNB  =  MINCNB
377300180924 001 C                   EVAL      O_MNTARC  =  MNTARC
377400180924 001 C                   EVAL      O_MBLK9C  =  MBLK9C
377500180924     C*
377600180924 001 C                   ENDSR
377700180924     C*
