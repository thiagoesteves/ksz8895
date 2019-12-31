%%%=============================================================================
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%=============================================================================

-ifndef(ksz8895).
-define(ksz8895, true).

%% Port Defines
-type ksz8895Instance() :: 0..4.
-type ksz8895PortId()   :: 1..5.

%% static information records
-record(ksz8895_data, {
  instance,
  family_id,
  chip_id,
  revision_id
}).

%% Global Configuration information: All default values match with the datasheet
-record(ksz8895_global_control, {
  %% Global Control 0
  new_back_off_en = 0,
  flush_dynamic_mac_table = 0,
  flush_static_mac_table = 0,
  enable_phy_mii = 1,
  unh_mode = 0,
  link_change_age = 0,
  %% Global Control 1
  pass_all_frames = 0,
  pckt_2k_byte_support = 0,
  ieee_tx_flow_control_dis = 0,
  ieee_rx_flow_control_dis = 0,
  frame_length_check = 0,
  aging_en = 1,
  fast_aging_en = 0,
  agressive_back_off_en = 0,
  %% Global Control 2
  unicast_vlan_mism_disc = 1,
  multicast_storm_protection_dis = 1,
  back_pressure_mode = 1,
  flow_control_bpm = 1,
  no_excessive_col_drop = 0,
  huge_pckt_support = 0,
  legal_maximum_size_check_dis = 0,
  %% Global Control 3
  vlan_802_1q_en = 0,
  igmp_snoop_en = 0,
  direct_mode_en = 0,
  pre_tag_en = 0,
  tag_mask_en = 0,
  sniff_mode = 0,
  %% Global Control 4/5
  s_mii_back_pressure_en = 0,
  s_mii_hal_duplex_en = 0,
  s_mii_flow_control_en = 0,
  s_mii_speed_en = 0,
  null_vid_replacement = 0,
  s_mii_broadcast_storm = 0,
  %% Global Control 9
  port5_sw5_ref_clock_edge_sel = 0,
  phy_power_save = 0,
  led_mode = 0,
  spi_read_sampling = 0,
  %% Global Control 10
  status_device = 1,
  cpu_interface_clock_sel = 0,
  enable_restore_preamble = 0,
  tail_tag_enable = 0,
  pass_flow_control = 0
}).

%% Port Configuration information: All default values match with the datasheet
-record(ksz8895_port_control, {
  port = 1,
  %% Port Control 0
  broadcast_storm_protection_en = 0,
  diff_serv_priority_class_en = 0,
  priority_802_1p_class_en = 0,
  port_based_priority_class_en = 0,
  tag_insertion = 0,
  tag_removal = 0,
  two_queues_split_en = 0,
  %% Port Control 1
  sniffer_port = 0,
  rx_sniff = 0,
  tx_sniff = 0,
  port_vlan_membership = 31,
  %% Port Control 2
  user_priority_ceiling = 0,
  ingress_vlan_filtering = 0,
  discard_non_pvid_packets = 0,
  force_flow_control = 0,
  back_pressure_en = 0,
  rx_en = 1,
  tx_en = 1,
  learning_dis = 0,
  %% Port Control 3/4
  default_tag_priority = 0,
  default_tag_cfi = 0,
  default_tag_vid = 1,
  %% Port Phy Special Control
  phy_force_link = 0,
  phy_power_save = 0,
  phy_remote_loopback = 0,
  %% Port Control 12
  disable_auto_negociation = 0,
  forced_speed = 1,
  forced_duplex = 0,
  advertised_flow_control_cap = 1,
  advertised_100bt_full_cap = 1,
  advertised_100bt_half_cap = 1,
  advertised_10bt_full_cap = 1,
  advertised_10bt_half_cap = 1,
  %% Port Control 13
  led_off = 0,
  tx_dis = 0,
  restart_auto_negotiation = 0,
  power_down = 0,
  disable_auto_mdi = 0,
  forced_mdi = 0,
  mac_loopback = 0,
  %% Port Control 14
  phy_loopback = 0,
  phy_isolate = 0,
  soft_reset = 0,
  force_link = 0,
  port_mode = 0
}).

%% Port Status information
-record(ksz8895_port_status, {
  port = 1,
  %% Port Status 0
  hp_mdix = 0,
  factory_testing = 0,
  polarity_reversed = 0,
  tx_flow_control_en = 0,
  rx_flow_control_en = 0,
  operation_speed = 0,
  operation_duplex = 0,
  %% Port Phy Special Control
  phy_force_link = 0,
  phy_power_save = 0,
  phy_remote_loopback = 0,
  %% Port Status 1
  mdix = 0,
  auto_negotiation_done = 0,
  link_good = 0,
  partner_flow_control_cap = 0,
  partner_100bt_full_cap = 0,
  partner_100bt_half_cap = 0,
  partner_10bt_full_cap = 0,
  partner_10bt_half_cap = 0,
  %% Port Status 2
  phy_loopback = 0,
  phy_isolate = 0,
  soft_reset = 0,
  force_link = 0,
  port_mode = 0
}).

%% Port MIB information
-record(ksz8895_port_mib, {
  port = 1,
  rxLoPriorityByte = 0,
  rxHiPriorityByte = 0,
  rxUndersizePkt = 0,
  rxFragments = 0,
  rxOversize = 0,
  rxJabbers = 0,
  rxSymbolError = 0,
  rxCRCerror = 0,
  rxAlignmentError = 0,
  rxControl8808Pkts = 0,
  rxPausePkts = 0,
  rxBroadcast = 0,
  rxMulticast = 0,
  rxUnicast = 0,
  rx64Octets = 0,
  rx65to127Octets = 0,
  rx128to255Octets = 0,
  rx256to511Octets = 0,
  rx512to1023Octets = 0,
  rx1024to1522Octets = 0,
  txLoPriorityByte = 0,
  txHiPriorityByte = 0,
  txLateCollision = 0,
  txPausePkts = 0,
  txBroadcastPkts = 0,
  txMulticastPkts = 0,
  txUnicastPkts = 0,
  txDeferred = 0,
  txTotalCollision = 0,
  txExcessiveCollision = 0,
  txSingleCollision = 0,
  txMultipleCollision = 0,
  rxDropPackets = 0,
  txDropPackets = 0
}).

-endif. %% ksz8895