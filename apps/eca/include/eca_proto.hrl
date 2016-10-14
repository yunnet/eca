-author("yunnet").
-record(objTrack,{
  gpsTime :: integer(),
  longitude :: integer(),
  latitude :: integer(),
  head :: integer(),
  speed :: integer(),
  height :: integer(),
  alarmState :: integer(),
  status :: integer()
}).


-record(rdtMsg, {
  msgId :: integer(),
  isSubPack :: boolean(),
  isRas :: boolean(),
  size :: integer(),
  commNo :: string(),
  msgSeq :: integer(),
  subCount :: integer(),
  subSeq :: integer(),
  data :: string()
}).

-record(objUpRaw, {
  genTime :: integer(),
  terminalType :: integer(),
  commNO :: string(),
  tunnel,  %% TCP(1)  | UDP(2)
  dataHex :: string()
}).