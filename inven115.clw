

                     MEMBER('inventar.clw')                ! This is a MEMBER module

repromov2 PROCEDURE

RejectRecord         LONG                                  !
LocalRequest         LONG                                  !
LocalResponse        LONG                                  !
FilesOpened          LONG                                  !
WindowOpened         LONG                                  !
RecordsToProcess     LONG,AUTO                             !
RecordsProcessed     LONG,AUTO                             !
RecordsPerCycle      LONG,AUTO                             !
RecordsThisCycle     LONG,AUTO                             !
PercentProgress      BYTE                                  !
RecordStatus         BYTE,AUTO                             !
OriginalRequest      LONG                                  !
WindowInitialized    LONG                                  !
ForceRefresh         LONG                                  !
EnhancedFocusManager EnhancedFocusClassType
Process:View         VIEW(MOVINV)
                     END
Progress:Thermometer BYTE
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         Do GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(Event:CloseWindow)
           CYCLE
         END
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
       RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP WHILE RecordsThisCycle < RecordsPerCycle
            TIP:CODIGO = MOV:TIPO
            get(tipmov,TIP:KeyCODIGO)
            
            CLEAR(ITE:RECORD)
            ITE:CODIGO=MOV:CODIGO
            get(items,ITE:KEYCODIGO)
            
            
            if mov:costo=0
               mov:costo = ite:costo
            .
            
            IF TIP:COSTO='SI'
               if ite:saldo > 0
                  ITE:COSTO = ((ITE:SALDO*ITE:COSTO)+(MOV:CANTIDAD*mov:COSTO))/(MOV:CANTIDAD+ite:saldo)
               else
                  ite:costo = MOV:COSTO
               .
            .
            
            
            EMOV:TIPO  = MOV:TIPO
            EMOV:NDOC  = MOV:NDOC
            GET(ENC_MOV,EMOV:keytipndoc)
            IF ERRORCODE()=35
               EMOV:NDOC = FORMAT(EMOV:NDOC,@N07)
               GET(ENC_MOV,EMOV:keytipndoc)
               if errorcode()=35
                  clear(emov:record)
                  EMOV:TIPO   = MOV:TIPO
                  EMOV:NDOC   = MOV:NDOC
                  EMOV:FECHA  = MOV:FECHA
                  ADD(ENC_MOV)
               .
            .
            
            if EMOV:TIPO = 'STR' OR EMOV:TIPO = 'XTR'
            ELSE
               EMOV:OPTICA =''
            .
            
            EMOV:NDOC = FORMAT(EMOV:NDOC,@N07)
            MOV:NDOC = FORMAT(MOV:NDOC,@N07)
            PUT(ENC_MOV)
            
            put(movinv)
            
            if TIP:SIGNO ='-'
               ITE:SALDO - = MOV:CANTIDAD
            ELSE
               ITE:SALDO += MOV:CANTIDAD
            .
            PUT(ITEMS)
          LOOP
             DO GetNextRecord
             DO ValidateRecord
             CASE RecordStatus
               OF Record:OutOfRange
                  LocalResponse = RequestCancelled
                  BREAK
               OF Record:OK
                  BREAK
             END
          END
          IF LocalResponse = RequestCancelled
             LocalResponse = RequestCompleted
             BREAK
          END
          LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
          0{PROP:Timer} = 0
          ?Progress:PctText{Prop:Text} = 'Process Completed'
          DISPLAY(?Progress:PctText)
          POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         0{PROP:Timer} = 0
         POST(EVENT:CloseWindow)
      END
    END
    EnhancedFocusManager.TakeEvent()
  END
  IF SEND(MOVINV,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'MOVINV')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(MOV:IFECHA)
  Process:View{Prop:Filter} = ''
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:ORDER}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
PrepareProcedure ROUTINE
  IF ENC_MOV::Used = 0
    CheckOpen(ENC_MOV,1)
  END
  ENC_MOV::Used += 1
  IF ITEMS::Used = 0
    CheckOpen(ITEMS,1)
  END
  ITEMS::Used += 1
  IF MOVINV::Used = 0
    CheckOpen(MOVINV,1)
  END
  MOVINV::Used += 1
  IF TIPMOV::Used = 0
    CheckOpen(TIPMOV,1)
  END
  TIPMOV::Used += 1
  FilesOpened = True
  Do BindFields
  STREAM(ITEMS)
  BUFFER(ITEMS,10,5,2,300)
  STREAM(MOVINV)
  STREAM(ENC_MOV)
  RecordsToProcess = RECORDS(MOVINV)
  RecordsPerCycle = 25
  RecordsProcessed = 0
  PercentProgress = 0
  OPEN(ProgressWindow)
  WindowOpened=True
  EnhancedFocusManager.Init(1,15263971,1,0,15263971,0,65535,0,2,65535,0,1,8421504,'»',8)
  INIRestoreWindow('repromov2','inventar.INI')
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}='REPROCESA MOVIMIENTO'
  SEND(MOVINV,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(EMOV:RECORD)
  BIND(ITE:RECORD)
  BIND(MOV:RECORD)
  BIND(TIP:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    ENC_MOV::Used -= 1
    IF ENC_MOV::Used = 0 THEN CLOSE(ENC_MOV).
    ITEMS::Used -= 1
    IF ITEMS::Used = 0 THEN CLOSE(ITEMS).
    MOVINV::Used -= 1
    IF MOVINV::Used = 0 THEN CLOSE(MOVINV).
    TIPMOV::Used -= 1
    IF TIPMOV::Used = 0 THEN CLOSE(TIPMOV).
  END
  IF WindowOpened
    INISaveWindow('repromov2','inventar.INI')
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  repromov3
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
