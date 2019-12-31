# The KSZ8895 application #

__Authors:__ Thiago Esteves ([`thiagocalori@gmail.com`](thiagocalori@gmail.com)).

## Note ##

The KSZ8895 application is dependent on GPROC library and the low level functions implemented at c_src/ksz8895_driver.c are just for test purposes (stubbed) and must be replace for the real accessors.

## Introduction ##

The KSZ8895MQX/RQX/FQX/ML is a highly-integrated, Layer 2 managed, five-port switch with numerous features designed to reduce system cost. The KSZ8895 is Automotive qualified (AEC-Q100).

### Compiling and Running ###

To compile and run for your machine just call the following command in the CLI:

```bash
$ make
```

### Use case: Creating KSZ8895 devices ###

The user can create as many device as needed (the stub supports only 20, but with the real hardware there is no limitation). In general, the projects will support only one which means all functions will use the default instance when its value is ommited:

```erlang
1> ksz8895_sup:create_ksz8895(0).
{ok,<0.167.0>}
2> ksz8895:get_state().
{ksz8895_data,0,149,10,7}
3> ksz8895:get_state(0).
{ksz8895_data,0,149,10,7}
```
### Global configuration records ###

The ksz8895 driver is using records in order to configure or read the global control registers, which means you can modify only one value or all of them in a single operation:

```erlang
1> ksz8895_sup:create_ksz8895(0).
{ok,<0.167.0>}
2> ksz8895:get_state().
{ksz8895_data,0,149,10,7}
3> %% load records at the shell
3> rr(ksz8895).
[ksz8895_data,ksz8895_global_control,ksz8895_port_control,
 ksz8895_port_status]
4> {ok, Global} = ksz8895:get_global_control().
{ok,#ksz8895_global_control{new_back_off_en = 0,
                            flush_dynamic_mac_table = 0,flush_static_mac_table = 0,
                            enable_phy_mii = 0,unh_mode = 0,link_change_age = 0,
                            pass_all_frames = 0,pckt_2k_byte_support = 0,
                            ieee_tx_flow_control_dis = 0,ieee_rx_flow_control_dis = 0,
                            frame_length_check = 0,aging_en = 0,fast_aging_en = 0,
                            agressive_back_off_en = 0,unicast_vlan_mism_disc = 0,
                            multicast_storm_protection_dis = 0,back_pressure_mode = 0,
                            flow_control_bpm = 0,no_excessive_col_drop = 0,
                            huge_pckt_support = 0,legal_maximum_size_check_dis = 0,
                            vlan_802_1q_en = 0,igmp_snoop_en = 0,direct_mode_en = 0,
                            pre_tag_en = 0,tag_mask_en = 0,...}}
5> ksz8895:set_global_control( Global#ksz8895_global_control{
5>   new_back_off_en = 1,
5>   flush_dynamic_mac_table = 1,
5>   flush_static_mac_table = 1} ).
{ok,0}
6> %% read back the configuration
6> ksz8895:get_global_control().
{ok,#ksz8895_global_control{new_back_off_en = 1,
                            flush_dynamic_mac_table = 1,flush_static_mac_table = 1,
                            enable_phy_mii = 0,unh_mode = 0,link_change_age = 0,
                            pass_all_frames = 0,pckt_2k_byte_support = 0,
                            ieee_tx_flow_control_dis = 0,ieee_rx_flow_control_dis = 0,
                            frame_length_check = 0,aging_en = 0,fast_aging_en = 0,
                            agressive_back_off_en = 0,unicast_vlan_mism_disc = 0,
                            multicast_storm_protection_dis = 0,back_pressure_mode = 0,
                            flow_control_bpm = 0,no_excessive_col_drop = 0,
                            huge_pckt_support = 0,legal_maximum_size_check_dis = 0,
                            vlan_802_1q_en = 0,igmp_snoop_en = 0,direct_mode_en = 0,
                            pre_tag_en = 0,tag_mask_en = 0,...}}
```

### Port configuration records ###

The Ksz8895 supports up to 5 ports and you can use the port_control records to read and write its configuration, The next example will configure the Port 2.

```erlang
1> ksz8895_sup:create_ksz8895(0).
{ok,<0.167.0>}
2> ksz8895:get_state().
{ksz8895_data,0,149,10,7}
3> %% load records at the shell
3> rr(ksz8895).
[ksz8895_data,ksz8895_global_control,ksz8895_port_control,
 ksz8895_port_status]
4> %% read the current configuration on P2
4> {ok, P2} = ksz8895:get_port_control(2).
{ok,#ksz8895_port_control{port = 2,
                          broadcast_storm_protection_en = 0,
                          diff_serv_priority_class_en = 0,
                          priority_802_1p_class_en = 0,
                          port_based_priority_class_en = 0,tag_insertion = 0,
                          tag_removal = 0,two_queues_split_en = 0,sniffer_port = 0,
                          rx_sniff = 0,tx_sniff = 0,port_vlan_membership = 0,
                          user_priority_ceiling = 0,ingress_vlan_filtering = 0,
                          discard_non_pvid_packets = 0,force_flow_control = 0,
                          back_pressure_en = 0,rx_en = 0,tx_en = 0,learning_dis = 0,
                          default_tag_priority = 0,default_tag_cfi = 0,
                          default_tag_vid = 0,phy_force_link = 0,phy_power_save = 0,
                          phy_remote_loopback = 1,...}}
5> ksz8895:set_port_control( P2#ksz8895_port_control{
5>   port = 2,
5>   broadcast_storm_protection_en = 1,
5>   diff_serv_priority_class_en = 1,
5>   priority_802_1p_class_en = 1,
5>   phy_loopback = 1,
5>   phy_isolate = 1,
5>   soft_reset = 1,
5>   force_link = 1,
5>   port_mode = 1} ).
{ok,0}
6> %% read back the port configuration
6> ksz8895:get_port_control(2).
{ok,#ksz8895_port_control{port = 2,
                          broadcast_storm_protection_en = 1,
                          diff_serv_priority_class_en = 1,
                          priority_802_1p_class_en = 1,
                          port_based_priority_class_en = 0,tag_insertion = 0,
                          tag_removal = 0,two_queues_split_en = 0,sniffer_port = 0,
                          rx_sniff = 0,tx_sniff = 0,port_vlan_membership = 0,
                          user_priority_ceiling = 0,ingress_vlan_filtering = 0,
                          discard_non_pvid_packets = 0,force_flow_control = 0,
                          back_pressure_en = 0,rx_en = 0,tx_en = 0,learning_dis = 0,
                          default_tag_priority = 0,default_tag_cfi = 0,
                          default_tag_vid = 0,phy_force_link = 0,phy_power_save = 0,
                          phy_remote_loopback = 1,...}}
```

### Mib counters ###

The Ksz8895 supports mib counters for all ports. The read operation will return all counters at the respective record counter:

```erlang
1> ksz8895_sup:create_ksz8895(0).
{ok,<0.167.0>}
2> ksz8895:get_state().
{ksz8895_data,0,149,10,7}
3> %% load records at the shell
3> rr(ksz8895).
[ksz8895_data,ksz8895_global_control,ksz8895_port_control,
 ksz8895_port_mib,ksz8895_port_status]
4> ksz8895:get_port_mib(1).
{ok,#ksz8895_port_mib{port = 1,rxLoPriorityByte = 1,
                      rxHiPriorityByte = 2,rxUndersizePkt = 3,rxFragments = 4,
                      rxOversize = 5,rxJabbers = 6,rxSymbolError = 7,
                      rxCRCerror = 8,rxAlignmentError = 9,rxControl8808Pkts = 10,
                      rxPausePkts = 11,rxBroadcast = 12,rxMulticast = 13,
                      rxUnicast = 14,rx64Octets = 15,rx65to127Octets = 16,
                      rx128to255Octets = 17,rx256to511Octets = 18,
                      rx512to1023Octets = 19,rx1024to1522Octets = 20,
                      txLoPriorityByte = 21,txHiPriorityByte = 22,
                      txLateCollision = 23,txPausePkts = 24,txBroadcastPkts = 25,...}}
```
### Erlang Code References ###
http://erlang.org/doc/tutorial/c_port.html

http://erlang.org/doc/reference_manual/ports.html

### KSZ8895 References ###
http://ww1.microchip.com/downloads/en/DeviceDoc/00002246A.pdf

https://www.microchip.com/wwwproducts/en/KSZ8895