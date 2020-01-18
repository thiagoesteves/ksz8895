%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the ksz8895 implementation as a gen_server. All the high-level
%%% functions are located here and the low level functions (i2c read 
%%% and gpio read) are accessed by PORT.
%%%
%%% This gen-server supports multiple devices connected and can be
%%% dynamically created/removed by the ksz8895_sup.
%%% @end
%%%-------------------------------------------------------------------

-module(ksz8895).

-behaviour(gen_server).

-author('Thiago Esteves').

-include("ksz8895.hrl").

%% gen_server exports
-export([init/1,
         start_link/1,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% Public API export where the Instance is required
-export([get_state/1,
         get_port_status/2,
         get_port_control/2,
         get_port_mib/2,
         get_global_control/1,
         set_port_control/2,
         set_global_control/2,
         set_start/1,
         set_reset/1]).

%% Public API export where the Instance is the default defined value
-export([get_state/0,
         get_port_status/1,
         get_port_control/1,
         get_port_mib/1,
         get_global_control/0,
         set_port_control/1,
         set_global_control/1,
         set_start/0,
         set_reset/0]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

-define(DEFAULT_INSTANCE, 0).

%%%===================================================================
%%% PIN Defines - See ksz8895_driver.c enumeration
%%%===================================================================

-define(KSZ8895_PIN_RESET    , 0).
-define(KSZ8895_MAX_PIN      , 1).

-define(MIN_PORT             , 1).
-define(MAX_PORT             , 5).

%%%===================================================================
%%% Register Defines
%%%===================================================================
-define(KSZ8895_REG_CHIP_ID0,          0).
-define(KSZ8895_REG_CHIP_ID1,          1).

-define(KSZ8895_REG_GLOBAL0,           2).
-define(KSZ8895_REG_GLOBAL1,           3).
-define(KSZ8895_REG_GLOBAL2,           4).
-define(KSZ8895_REG_GLOBAL3,           5).
-define(KSZ8895_REG_GLOBAL4,           6).
-define(KSZ8895_REG_GLOBAL5,           7).
-define(KSZ8895_REG_GLOBAL9,           11).
-define(KSZ8895_REG_GLOBAL10,          12).

-define(KSZ8895_REG_PORT1_CONTROL0,    16).
-define(KSZ8895_REG_PORT1_CONTROL1,    17).
-define(KSZ8895_REG_PORT1_CONTROL2,    18).
-define(KSZ8895_REG_PORT1_CONTROL3,    19).
-define(KSZ8895_REG_PORT1_CONTROL4,    20).
-define(KSZ8895_REG_PORT1_PHY_CONTROL, 26).
-define(KSZ8895_REG_PORT1_CONTROL12,   28).
-define(KSZ8895_REG_PORT1_CONTROL13,   29).
-define(KSZ8895_REG_PORT1_CONTROL14,   31).

-define(KSZ8895_REG_PORT1_STATUS0,     25).
-define(KSZ8895_REG_PORT1_PHY_STATUS,  26).
-define(KSZ8895_REG_PORT1_STATUS1,     30).
-define(KSZ8895_REG_PORT1_STATUS2,     31).

%% Mib registers
-define(KSZ8895_REG_INDIRECT_ACCESS1,  110).
-define(KSZ8895_REG_INDIRECT_ACCESS2,  111).

-define(KSZ8895_REG_IA_VAL3,           117).
-define(KSZ8895_REG_IA_VAL2,           118).
-define(KSZ8895_REG_IA_VAL1,           119).
-define(KSZ8895_REG_IA_VAL0,           120).

-define(KSZ8895_REG_MIB_PORT_READ,     28).
-define(KSZ8895_REG_MIB_PORT_DROP_READ,29).
-define(KSZ8895_REG_MIB_PORT_OFFSET,   32).

-define(KSZ8895_REG_MIB_TX_DROP_OFFSET,0).
-define(KSZ8895_REG_MIB_RX_DROP_OFFSET,5).

-define(KSZ8895_MIB_COUNTER_OVERFLOW,  16#40000000).

-define(KSZ8895_REG_MIB_FIRST_OFFSET,  0).
-define(KSZ8895_REG_MIB_LAST_OFFSET,   31).

%% Step between Port Control/Status
-define(KSZ8895_REG_PORT_STEP,         16).

%% Reset delay im ms
-define(KSZ8895_RESET_DELAY,           100).

%%%===================================================================
%%% Bitfields defines PORT_CONTROL
%%%===================================================================
-define(PORT_CONTROL_LIST, [control_0, control_1, control_2, control_3, 
  control_4, control_phy, control_12, control_13, control_14]).

-define(PC0_RECORD_REG, #ksz8895_port_control { 
  broadcast_storm_protection_en = B,
  diff_serv_priority_class_en = D,
  priority_802_1p_class_en = P,
  port_based_priority_class_en = Po,
  tag_insertion = Tin,
  tag_removal = Trem,
  two_queues_split_en = Two} ).
-define(PC0_BITFIELD_REG, <<B:1,D:1,P:1,Po:2,Tin:1,Trem:1,Two:1>>).

-define(PC1_RECORD_REG, #ksz8895_port_control {
  sniffer_port = S,
  rx_sniff = R,
  tx_sniff = T,
  port_vlan_membership = V} ).
-define(PC1_BITFIELD_REG, <<S:1,R:1,T:1,V:5>>).

-define(PC2_RECORD_REG, #ksz8895_port_control {
  user_priority_ceiling = U,
  ingress_vlan_filtering = I,
  discard_non_pvid_packets = D,
  force_flow_control = F,
  back_pressure_en = B,
  rx_en = R,
  tx_en = T,
  learning_dis = L} ).
-define(PC2_BITFIELD_REG, <<U:1,I:1,D:1,F:1,B:1,R:1,T:1,L:1>>).

-define(PC3_RECORD_REG, #ksz8895_port_control {
  default_tag_priority = P,
  default_tag_cfi = C,
  default_tag_vid = V} ).
-define(PC3_BITFIELD_REG, <<P:3,C:1,VidMsb:4>>).

-define(PC4_RECORD_REG, #ksz8895_port_control {
  default_tag_vid = V} ).
-define(PC4_BITFIELD_REG, <<V:8>>).

-define(PCPHY_RECORD_REG, #ksz8895_port_control {
  phy_force_link = F,
  phy_power_save = P,
  phy_remote_loopback = R} ).
-define(PCPHY_BITFIELD_REG, <<0:4,F:1,P:1,R:1,0:1>>).

-define(PC12_RECORD_REG, #ksz8895_port_control {
  disable_auto_negociation = DAN,
  forced_speed = S,
  forced_duplex = D,
  advertised_flow_control_cap = AF,
  advertised_100bt_full_cap = A100F,
  advertised_100bt_half_cap = A100H,
  advertised_10bt_full_cap = A10F,
  advertised_10bt_half_cap = A10H} ).
-define(PC12_BITFIELD_REG, <<DAN:1,S:1,D:1,AF:1,A100F:1,A100H:1,A10F:1,A10H:1>>).

-define(PC13_RECORD_REG, #ksz8895_port_control {
  led_off = L,
  tx_dis = T,
  restart_auto_negotiation = RAN,
  power_down = P,
  disable_auto_mdi = D,
  forced_mdi = F,
  mac_loopback = M} ).
-define(PC13_BITFIELD_REG, <<L:1,T:1,RAN:1,0:1,P:1,D:1,F:1,M:1>>).

-define(PC14_RECORD_REG, #ksz8895_port_control {
  phy_loopback = PL,
  phy_isolate = PI,
  soft_reset = S,
  force_link = F,
  port_mode = PM} ).
-define(PC14_BITFIELD_REG, <<PL:1,0:1,PI:1,S:1,F:1,PM:3>>).

%%%===================================================================
%%% Bitfields defines GLOBAL_CONTROL
%%%===================================================================
-define(GLOBAL_CONTROL_LIST, [global_0, global_1, global_2, global_3, 
  global_4, global_5, global_9, global_10]).

-define(GL0_RECORD_REG, #ksz8895_global_control {
  new_back_off_en = N,
  flush_dynamic_mac_table = FD,
  flush_static_mac_table = FS,
  enable_phy_mii = E,
  unh_mode = U,
  link_change_age = L} ).
-define(GL0_BITFIELD_REG, <<N:1,0:1,FD:1,FS:1,E:1,0:1,U:1,L:1>>).

-define(GL1_RECORD_REG, #ksz8895_global_control {
  pass_all_frames = P,
  pckt_2k_byte_support = P2K,
  ieee_tx_flow_control_dis = I3ETX,
  ieee_rx_flow_control_dis = I3ERX,
  frame_length_check = Fr,
  aging_en = A,
  fast_aging_en = Fa,
  agressive_back_off_en = Ag} ).
-define(GL1_BITFIELD_REG, <<P:1,P2K:1,I3ETX:1,I3ERX:1,Fr:1,A:1,Fa:1,Ag:1>>).

-define(GL2_RECORD_REG, #ksz8895_global_control {
  unicast_vlan_mism_disc = Un,
  multicast_storm_protection_dis = M,
  back_pressure_mode = B,
  flow_control_bpm = F,
  no_excessive_col_drop = N,
  huge_pckt_support = H,
  legal_maximum_size_check_dis = L} ).
-define(GL2_BITFIELD_REG, <<Un:1,M:1,B:1,F:1,N:1,H:1,L:1,0:1>>).

-define(GL3_RECORD_REG, #ksz8895_global_control {
  vlan_802_1q_en = V,
  igmp_snoop_en = I,
  direct_mode_en = D,
  pre_tag_en = P,
  tag_mask_en = T,
  sniff_mode = S} ).
-define(GL3_BITFIELD_REG, <<V:1,I:1,D:1,P:1,0:2,T:1,S:1>>).

-define(GL4_RECORD_REG, #ksz8895_global_control {
  s_mii_back_pressure_en = B,
  s_mii_hal_duplex_en = H,
  s_mii_flow_control_en = F,
  s_mii_speed_en = S,
  null_vid_replacement = N,
  s_mii_broadcast_storm = BS} ).
-define(GL4_BITFIELD_REG, <<B:1,H:1,F:1,S:1,N:1,Br:3>>).

-define(GL5_RECORD_REG, #ksz8895_global_control {
  s_mii_broadcast_storm = BS} ).
-define(GL5_BITFIELD_REG, <<BS:8>>).

-define(GL9_RECORD_REG, #ksz8895_global_control {
  port5_sw5_ref_clock_edge_sel = P,
  phy_power_save = Phy,
  led_mode = L,
  spi_read_sampling = S} ).
-define(GL9_BITFIELD_REG, <<0:1,P:1,0:2,Phy:1,0:1,L:1,S:1>>).

-define(GL10_RECORD_REG, #ksz8895_global_control {
  status_device = S,
  cpu_interface_clock_sel = C,
  enable_restore_preamble = E,
  tail_tag_enable = T,
  pass_flow_control = P} ).
-define(GL10_BITFIELD_REG, <<0:1,S:1,C:2,0:1,E:1,T:1,P:1>>).

%%%===================================================================
%%% Bitfields defines PORT_STATUS
%%%===================================================================

-define(PS0_RECORD_REG, #ksz8895_port_status {
  hp_mdix = H,
  factory_testing = F,
  polarity_reversed = P,
  tx_flow_control_en = T,
  rx_flow_control_en = R,
  operation_speed = OS,
  operation_duplex = OD} ).
-define(PS0_BITFIELD_REG, <<H:1,F:1,P:1,T:1,R:1,OS:1,OD:1,0:1>>).

-define(PSPHY_RECORD_REG, #ksz8895_port_status {
    phy_force_link = PF,
    phy_power_save = PP,
    phy_remote_loopback = PR} ).
-define(PSPHY_BITFIELD_REG, <<0:4,PF:1,PP:1,PR:1,0:1>>).

-define(PS1_RECORD_REG, #ksz8895_port_status {
    mdix = MDIX,
    auto_negotiation_done = AN,
    link_good = LG,
    partner_flow_control_cap = PFC,
    partner_100bt_full_cap = P100F,
    partner_100bt_half_cap = P100H,
    partner_10bt_full_cap = P10F,
    partner_10bt_half_cap = P10H} ).
-define(PS1_BITFIELD_REG, <<MDIX:1,AN:1,LG:1,PFC:1,P100F:1,P100H:1,P10F:1,P10H:1>>).

-define(PS2_RECORD_REG, #ksz8895_port_status {
    phy_loopback = PHYL,
    phy_isolate = PHYI,
    soft_reset = SR,
    force_link = FL,
    port_mode = PM} ).
-define(PS2_BITFIELD_REG, <<PHYL:1,_Res:1,PHYI:1,SR:1,FL:1,PM:3>>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link([Ksz8895Name, Instance]) ->
  gen_server:start_link({local, Ksz8895Name}, ?MODULE, [Instance], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([Instance]) ->
  State = #ksz8895_data { instance = Instance },
  %% Register Gproc name
  gproc:reg({p, l, {?MODULE, Instance}}),
  %% comment this line to stop trapping exits
  process_flag(trap_exit, true),
  %% Update State
  NewState = update_ksz8895_state(State),
  {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_, _LD) ->
  gproc:goodbye().

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
%% Get operations with no arguments
handle_call({get, state}, _From, State) ->
  {reply, State, State};

%% Set operations with no arguments
handle_call({set, Operation}, _From, State) ->
  Res = write_priv(Operation, State#ksz8895_data.instance),
  {reply, Res, State};

%% Get operations with arguments
handle_call({get_port_status, PortId}, _From, State) ->
  Res = read_port_status(State#ksz8895_data.instance, PortId),
  {reply, Res, State};

handle_call({get_global_control}, _From, State) ->
  Res = read_global_control(State#ksz8895_data.instance),
  {reply, Res, State};

handle_call({get_port_control, PortId}, _From, State) ->
  Res = read_port_control(State#ksz8895_data.instance, PortId),
  {reply, Res, State};

handle_call({get_port_mib, PortId}, _From, State) ->
  Res = read_port_mib(State#ksz8895_data.instance, PortId),
  {reply, Res, State};

%% Set operations with arguments
handle_call({set_port_control, Port_Control}, _From, State) ->
  Res = write_port_control(State#ksz8895_data.instance, Port_Control),
  {reply, Res, State};

handle_call({set_global_control, Global_Control}, _From, State) ->
  Res = write_global_control(State#ksz8895_data.instance, Global_Control),
  {reply, Res, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Exported KSZ8895 functions
%%%===================================================================
-spec get_state() -> { ok | error , #ksz8895_data{} }.
get_state() ->
  get_state(?DEFAULT_INSTANCE).

-spec get_state(ksz8895Instance()) -> { ok | error , #ksz8895_data{} }.
get_state(Instance) ->
  gproc_call(Instance, {get, state}).

-spec get_port_status(ksz8895PortId()) -> { ok | error , #ksz8895_port_status{} }.
get_port_status(PortId) ->
  get_port_status(?DEFAULT_INSTANCE, PortId).

-spec get_port_status(ksz8895Instance(), ksz8895PortId()) -> { ok | error , #ksz8895_port_status{} }.
get_port_status(Instance, PortId) ->
  gproc_call(Instance, {get_port_status, PortId}).

-spec get_port_control(ksz8895PortId()) -> { ok | error , #ksz8895_port_status{} }.
get_port_control(PortId) ->
  get_port_control(?DEFAULT_INSTANCE, PortId).

-spec get_port_control(ksz8895Instance(), ksz8895PortId()) -> { ok | error , #ksz8895_port_status{} }.
get_port_control(Instance, PortId) ->
  gproc_call(Instance, {get_port_control, PortId}).

-spec get_port_mib(ksz8895PortId()) -> { ok | error , #ksz8895_port_mib{} }.
get_port_mib(PortId) ->
  get_port_mib(?DEFAULT_INSTANCE, PortId).

-spec get_port_mib(ksz8895Instance(), ksz8895PortId()) -> { ok | error , #ksz8895_port_mib{} }.
get_port_mib(Instance, PortId) ->
  gproc_call(Instance, {get_port_mib, PortId}).

-spec get_global_control() -> { ok | error , #ksz8895_global_control{} }.
get_global_control() ->
  get_global_control(?DEFAULT_INSTANCE).

-spec get_global_control(ksz8895Instance()) -> { ok | error , #ksz8895_global_control{} }.
get_global_control(Instance) ->
  gproc_call(Instance, {get_global_control}).

-spec set_port_control(#ksz8895_port_control{}) -> { ok | error , integer() }.
set_port_control(Port_Control) ->
  set_port_control(?DEFAULT_INSTANCE, Port_Control).

-spec set_port_control(ksz8895Instance(), #ksz8895_port_control{}) -> { ok | error , integer() }.
set_port_control(Instance, Port_Control) ->
  gproc_call(Instance, {set_port_control, Port_Control}).

-spec set_global_control(#ksz8895_global_control{}) -> { ok | error , integer() }.
set_global_control(Global_Control) ->
  set_global_control(?DEFAULT_INSTANCE, Global_Control).

-spec set_global_control(ksz8895Instance(), #ksz8895_global_control{}) -> { ok | error , integer() }.
set_global_control(Instance, Global_Control) ->
  gproc_call(Instance, {set_global_control, Global_Control}).

-spec set_reset() -> { ok | error , integer() }.
set_reset() ->
  set_reset(?DEFAULT_INSTANCE).

-spec set_reset(ksz8895Instance()) -> { ok | error , integer() }.
set_reset(Instance) ->
  gproc_call(Instance, {set, reset}).

-spec set_start() -> { ok | error , integer() }.
set_start() ->
  set_start(?DEFAULT_INSTANCE).

-spec set_start(ksz8895Instance()) -> { ok | error , integer() }.
set_start(Instance) ->
  gproc_call(Instance, {set, start}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private Update ksz8895 static data
%%--------------------------------------------------------------------
update_ksz8895_state(S) ->
  %% 1 byte information data
  {ok, ChipId0} = ksz8895_driver:read_register(S#ksz8895_data.instance, ?KSZ8895_REG_CHIP_ID0),
  {ok, ChipId1} = ksz8895_driver:read_register(S#ksz8895_data.instance, ?KSZ8895_REG_CHIP_ID1),

  <<ChipId:4, RevId:3, _Start:1>> = <<ChipId1>>,
  %% Update all information but instance
  S#ksz8895_data {
    family_id   = ChipId0,
    chip_id     = ChipId,
    revision_id = RevId
  }.

%%--------------------------------------------------------------------
%% @private Write ksz8895 information
%%--------------------------------------------------------------------
write_priv(start, Instance) ->
  ksz8895_driver:write_register(Instance, ?KSZ8895_REG_CHIP_ID1, 1);

write_priv(reset, Instance) ->
  ksz8895_driver:write_pin(Instance, ?KSZ8895_PIN_RESET, 0),
  timer:sleep(?KSZ8895_RESET_DELAY),
  ksz8895_driver:write_pin(Instance, ?KSZ8895_PIN_RESET, 1).

%%--------------------------------------------------------------------
%% @private Read/Write KSZ8895 Global Control
%%--------------------------------------------------------------------

write_global_control(Instance, PG) ->
  RegisterToUpdate = ?GLOBAL_CONTROL_LIST,
  %% Compose all register and update
  [ global_control_to_register(Reg, Instance, PG) || Reg <-RegisterToUpdate ],
  {ok, 0}.

read_global_control(Instance) ->
  RegisterToRead = ?GLOBAL_CONTROL_LIST,
  %% Read and compose port_control
  GlobalControl = lists:foldl(
    fun (Reg, AccIn) ->
      register_to_global_control(Reg, Instance, AccIn)
    end,
    #ksz8895_global_control{},
    RegisterToRead),
  {ok, GlobalControl}.

%%--------------------------------------------------------------------
%% @private Convert record data (global_control) to/from register
%%          Defines were used here to reused for read and write operation
%%--------------------------------------------------------------------
global_control_to_register(global_0, Instance, ?GL0_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL0, 
    b2du(?GL0_BITFIELD_REG));

global_control_to_register(global_1, Instance, ?GL1_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL1, 
    b2du(?GL1_BITFIELD_REG));

global_control_to_register(global_2, Instance, ?GL2_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL2, 
    b2du(?GL2_BITFIELD_REG));

global_control_to_register(global_3, Instance, ?GL3_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL3, 
    b2du(?GL3_BITFIELD_REG));

global_control_to_register(global_4, Instance, ?GL4_RECORD_REG) ->
  <<_Rest1:21,Br:3,_Rest2:8>> = pad_to_32(b2eu(BS)),
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL4, 
    b2du(?GL4_BITFIELD_REG));

global_control_to_register(global_5, Instance, ?GL5_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL5, 
    b2du(?GL5_BITFIELD_REG));

global_control_to_register(global_9, Instance, ?GL9_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL9, 
    b2du(?GL9_BITFIELD_REG));

global_control_to_register(global_10, Instance, ?GL10_RECORD_REG) ->
  ksz8895_driver:write_register(
    Instance, 
    ?KSZ8895_REG_GLOBAL10, 
    b2du(?GL10_BITFIELD_REG)).

register_to_global_control(global_0, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL0),
  %% Convert register to records
  ?GL0_BITFIELD_REG = <<Val>>,
  PG?GL0_RECORD_REG;

register_to_global_control(global_1, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL1),
  %% Convert register to records
  ?GL1_BITFIELD_REG = <<Val>>,
  PG?GL1_RECORD_REG;

register_to_global_control(global_2, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL2),
  %% Convert register to records
  ?GL2_BITFIELD_REG = <<Val>>,
  PG?GL2_RECORD_REG;

register_to_global_control(global_3, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL3),
  %% Convert register to records
  ?GL3_BITFIELD_REG = <<Val>>,
  PG?GL3_RECORD_REG;

register_to_global_control(global_4, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL4),
  %% Convert register to records
  ?GL4_BITFIELD_REG = <<Val>>,
  BS = b2du(<<0:5,Br:3,0:8>>),  % needed for compatibility
  PG?GL4_RECORD_REG;

register_to_global_control(global_5, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL5),
  %% Convert register to records - expect to read control_3 first
  BS = PG#ksz8895_global_control.s_mii_broadcast_storm + Val,
  PG?GL5_RECORD_REG;

register_to_global_control(global_9, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL9),
  %% Convert register to records
  ?GL9_BITFIELD_REG = <<Val>>,
  PG?GL9_RECORD_REG;

register_to_global_control(global_10, Instance, PG) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    ?KSZ8895_REG_GLOBAL10),
  %% Convert register to records
  ?GL10_BITFIELD_REG = <<Val>>,
  PG?GL10_RECORD_REG.

%%--------------------------------------------------------------------
%% @private Read/Write KSZ8895 Port Control
%%--------------------------------------------------------------------
write_port_control(Instance, PC) when PC#ksz8895_port_control.port >= ?MIN_PORT,
                                      PC#ksz8895_port_control.port =< ?MAX_PORT ->
  RegisterToUpdate = ?PORT_CONTROL_LIST,
  %% Compose all register and update
  [ port_control_to_register(Reg, Instance, PC) || Reg <-RegisterToUpdate ],
  {ok, 0};

write_port_control(_, _) ->
  {error, invalid_port}.

read_port_control(Instance, PortId) when PortId >= ?MIN_PORT,
                                         PortId =< ?MAX_PORT ->
  RegisterToRead = ?PORT_CONTROL_LIST,
  %% Read and compose port_control
  PortControl = lists:foldl(
    fun (Reg, AccIn) ->
      register_to_port_control(Reg, Instance, AccIn)
    end,
    #ksz8895_port_control{port = PortId},
    RegisterToRead),
  {ok, PortControl};

read_port_control(_, _) ->
  {error, invalid_port}.

%%--------------------------------------------------------------------
%% @private Convert record data (port_control) to/from register
%%          Defines were used here to reused for read and write operation
%%--------------------------------------------------------------------

port_control_to_register(control_0, Instance, ?PC0_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, ?KSZ8895_REG_PORT1_CONTROL0), 
    b2du(?PC0_BITFIELD_REG));

port_control_to_register(control_1, Instance, ?PC1_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL1), 
    b2du(?PC1_BITFIELD_REG));

port_control_to_register(control_2, Instance, ?PC2_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL2),
    b2du(?PC2_BITFIELD_REG));

port_control_to_register(control_3, Instance, ?PC3_RECORD_REG = PC ) ->
  <<_Rest1:20,VidMsb:4,_Rest2:8>> = pad_to_32(b2eu(V)),
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL3),
    b2du(?PC3_BITFIELD_REG));

port_control_to_register(control_4, Instance, ?PC4_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL4),
    b2du(?PC4_BITFIELD_REG));

port_control_to_register(control_phy, Instance, ?PCPHY_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_PHY_CONTROL),
    b2du(?PCPHY_BITFIELD_REG));

port_control_to_register(control_12, Instance, ?PC12_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port,
                      ?KSZ8895_REG_PORT1_CONTROL12),
    b2du(?PC12_BITFIELD_REG));

port_control_to_register(control_13, Instance, ?PC13_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL13),
    b2du(?PC13_BITFIELD_REG));

port_control_to_register(control_14, Instance, ?PC14_RECORD_REG = PC ) ->
  ksz8895_driver:write_register(
    Instance, 
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL14),
    b2du(?PC14_BITFIELD_REG)).


register_to_port_control(control_0, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL0)),
  %% Convert register to records
  ?PC0_BITFIELD_REG = <<Val>>,
  PC?PC0_RECORD_REG;

register_to_port_control(control_1, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL1)),
  %% Convert register to records
  ?PC1_BITFIELD_REG = <<Val>>,
  PC?PC1_RECORD_REG;

register_to_port_control(control_2, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL2)),
  %% Convert register to records
  ?PC2_BITFIELD_REG = <<Val>>,
  PC?PC2_RECORD_REG;

register_to_port_control(control_3, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL3)),
  %% Convert register to records
  ?PC3_BITFIELD_REG = <<Val>>,
  V = b2du(<<0:4,VidMsb:4,0:8>>),  % needed for compatibility
  PC?PC3_RECORD_REG;

register_to_port_control(control_4, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL4)),
  %% Convert register to records - expect to read control_3 first
  V = PC#ksz8895_port_control.default_tag_vid + Val,
  PC?PC4_RECORD_REG;

register_to_port_control(control_phy, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_PHY_CONTROL)),
  %% Convert register to records
  ?PCPHY_BITFIELD_REG = <<Val>>,
  PC?PCPHY_RECORD_REG;

register_to_port_control(control_12, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL12)),
  %% Convert register to records
  ?PC12_BITFIELD_REG = <<Val>>,
  PC?PC12_RECORD_REG;

register_to_port_control(control_13, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL13)),
  %% Convert register to records
  ?PC13_BITFIELD_REG = <<Val>>,
  PC?PC13_RECORD_REG;

register_to_port_control(control_14, Instance, PC) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PC#ksz8895_port_control.port, 
                      ?KSZ8895_REG_PORT1_CONTROL14)),
  %% Convert register to records
  ?PC14_BITFIELD_REG = <<Val>>,
  PC?PC14_RECORD_REG.

%%--------------------------------------------------------------------
%% @private Read KSZ8895 Port Status
%%--------------------------------------------------------------------
read_port_mib(Instance, PortId) when PortId >= ?MIN_PORT,
                                     PortId =< ?MAX_PORT ->
  %% Read all MIB registers by port and in sequence with the record ksz8895_port_mib
  MibRecord = lists:foldl(
    fun (Reg, Acc0) ->
      V = read_mib_register(Instance, PortId, Reg),
      erlang:append_element(Acc0, V)
    end,
    {ksz8895_port_mib, PortId},
    lists:seq(?KSZ8895_REG_MIB_FIRST_OFFSET, ?KSZ8895_REG_MIB_LAST_OFFSET)),
  % Add extra drop packets in the record
  R = lists:foldl(
    fun (Reg, Acc0) ->
      V = read_mib_register_drop_packet(Instance, PortId, Reg),
      erlang:append_element(Acc0, V)
    end,
    MibRecord,
    [?KSZ8895_REG_MIB_RX_DROP_OFFSET, ?KSZ8895_REG_MIB_TX_DROP_OFFSET]),
  {ok, R};

read_port_mib(_, _) ->
  {error, invalid_port}.

%%--------------------------------------------------------------------
%% @private Convert register to port_mib record
%%--------------------------------------------------------------------
read_mib_register(Inst, Port, Reg) ->
  %% Prepare indirect access
  {ok, _} = ksz8895_driver:write_register(Inst,
                                          ?KSZ8895_REG_INDIRECT_ACCESS1,
                                          ?KSZ8895_REG_MIB_PORT_READ),
  {ok, _} = ksz8895_driver:write_register(Inst,
                                          ?KSZ8895_REG_INDIRECT_ACCESS2,
                                          calculate_mib_address(Port, Reg)),
  %% Read the value using indirect access
  {ok, Val3} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL3),
  {ok, Val2} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL2),
  {ok, Val1} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL1),
  {ok, Val0} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL0),
  %% compose bitfield
  <<Overflow:1,Valid:1,Counter:30>> = <<Val3:8, Val2:8, Val1:8,Val0:8>>,
  %% Check valid number
  case {Valid,Overflow} of
    {1, 0} -> Counter;
    {1, 1} -> Counter + ?KSZ8895_MIB_COUNTER_OVERFLOW;
    {0, _} -> 0
  end.

read_mib_register_drop_packet(Inst, Port, Reg) ->
  %% Prepare indirect access
  {ok, _} = ksz8895_driver:write_register(Inst,
                                          ?KSZ8895_REG_INDIRECT_ACCESS1,
                                          ?KSZ8895_REG_MIB_PORT_DROP_READ),
  {ok, _} = ksz8895_driver:write_register(Inst,
                                          ?KSZ8895_REG_INDIRECT_ACCESS2,
                                          ((Port-1) + Reg)),
  %% Read the value using indirect access
  {ok, Val1} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL1),
  {ok, Val0} = ksz8895_driver:read_register(Inst, ?KSZ8895_REG_IA_VAL0),
  %% compose bitfield
  <<Counter:16>> = <<Val1:8,Val0:8>>,
  Counter.

%%--------------------------------------------------------------------
%% @private Read KSZ8895 Port Status
%%--------------------------------------------------------------------
read_port_status(Instance, PortId) when PortId >= ?MIN_PORT,
                                        PortId =< ?MAX_PORT ->
  RegisterToRead = [status_0, status_1, status_2, status_phy],
  %% Read and compose port_status
  PortStatus = lists:foldl(
    fun (Reg, AccIn) ->
      register_to_port_status(Reg, Instance, AccIn)
    end,
    #ksz8895_port_status{port = PortId},
    RegisterToRead),
  {ok, PortStatus};

read_port_status(_, _) ->
  {error, invalid_port}.

%%--------------------------------------------------------------------
%% @private Convert register to port_status record
%%--------------------------------------------------------------------
register_to_port_status(status_0, Instance, PS) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PS#ksz8895_port_status.port, 
                      ?KSZ8895_REG_PORT1_STATUS0)),
  %% Convert register to records
  ?PS0_BITFIELD_REG = <<Val>>,
  PS?PS0_RECORD_REG;

register_to_port_status(status_phy, Instance, PS) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PS#ksz8895_port_status.port, 
                      ?KSZ8895_REG_PORT1_PHY_STATUS)),
  %% Convert register to records
  ?PSPHY_BITFIELD_REG = <<Val>>,
  PS?PSPHY_RECORD_REG;

register_to_port_status(status_1, Instance, PS) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PS#ksz8895_port_status.port, 
                      ?KSZ8895_REG_PORT1_STATUS1)),
  %% Convert register to records
  ?PS1_BITFIELD_REG = <<Val>>,
  PS?PS1_RECORD_REG;

register_to_port_status(status_2, Instance, PS) ->
  {ok, Val} = ksz8895_driver:read_register(
    Instance,
    calculate_address(PS#ksz8895_port_status.port, 
                      ?KSZ8895_REG_PORT1_STATUS2)),
  %% Convert register to records
  ?PS2_BITFIELD_REG = <<Val>>,
  PS?PS2_RECORD_REG.

%%--------------------------------------------------------------------
%% @private Send a gen_server:call message if the PID is found
%%--------------------------------------------------------------------
gproc_call(Instance, Msg) ->
  Key = {?MODULE, Instance},
  case gproc:lookup_pids({p, l, Key}) of
    [Pid] -> gen_server:call(Pid, Msg);
    _ -> {error, invalid_ksz8895}
  end.

calculate_address(Port, BaseAddress) ->
  (BaseAddress + ((Port - 1) * ?KSZ8895_REG_PORT_STEP)).

calculate_mib_address(Port, BaseAddress) ->
  (BaseAddress + ((Port - 1) * ?KSZ8895_REG_MIB_PORT_OFFSET)).

b2eu(Val) -> 
  binary:encode_unsigned(Val).

b2du(Bin) -> 
  binary:decode_unsigned(Bin).

pad_to_32(Bin) ->
  case (4 - size(Bin)) of
    0 -> Bin;
    N -> <<0:(N*8), Bin/binary>>
  end.
















