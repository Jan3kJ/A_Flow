advanced_shot {{exit_if 1 flow 8.0 volume 100 max_flow_or_pressure_range 0.6 transition fast popup {} exit_flow_under 0 temperature 93 weight 0.0 name Fill pressure 3.0 sensor coffee pump flow exit_type pressure_over exit_flow_over 6 max_flow_or_pressure 8.0 exit_pressure_over 3.00 exit_pressure_under 0 seconds 15} {exit_if 0 flow 0.0 volume 100 max_flow_or_pressure_range 0.6 transition fast popup {$weight} exit_flow_under 0 temperature 93 weight 4.0 name Infuse pressure 3.0 pump pressure sensor coffee exit_type pressure_over exit_flow_over 6 exit_pressure_over 3.0 max_flow_or_pressure 1.0 seconds 60.0 exit_pressure_under 0} {exit_if 1 flow 8 volume 100 max_flow_or_pressure_range 0.6 transition smooth popup {$weight} exit_flow_under 0 temperature 93 weight 0.0 name {Pressure Up} pressure 10.0 sensor coffee pump pressure exit_type flow_over exit_flow_over 2.0 max_flow_or_pressure 0 exit_pressure_over 8.5 exit_pressure_under 0 seconds 0} {exit_if 1 flow 8 volume 100 max_flow_or_pressure_range 0.6 transition smooth popup {$weight} exit_flow_under 2.1 temperature 93 weight 0.0 name {Pressure Decline} pressure 1.0 sensor coffee pump pressure exit_type flow_under exit_flow_over 3.00 max_flow_or_pressure 0 exit_pressure_over 11 exit_pressure_under 1 seconds 0} {exit_if 1 flow 2.0 volume 100 max_flow_or_pressure_range 0.6 transition fast popup {Flow Start} exit_flow_under 0 temperature 93 weight 0.0 name {Flow Start} pressure 3.0 pump flow sensor coffee exit_type flow_over exit_flow_over 1.9 exit_pressure_over 11 max_flow_or_pressure 0 seconds 10 exit_pressure_under 0} {exit_if 0 flow 4.0 volume 100 max_flow_or_pressure_range 0.6 transition smooth popup {$weight} exit_flow_under 0 temperature 93 weight 0.0 name {Flow Extraction} pressure 3.0 pump flow sensor coffee exit_type pressure_under exit_flow_over 6 exit_pressure_over 11 max_flow_or_pressure 10.0 seconds 60 exit_pressure_under 0}}
espresso_temperature_steps_enabled 0
author Janek
read_only 0
read_only_backup {}
espresso_hold_time 15
preinfusion_time 20
espresso_pressure 6.0
espresso_decline_time 30
pressure_end 4.0
espresso_temperature 92.0
espresso_temperature_0 92.0
espresso_temperature_1 92.0
espresso_temperature_2 92.0
espresso_temperature_3 92.0
settings_profile_type settings_2c
flow_profile_preinfusion 4
flow_profile_preinfusion_time 5
flow_profile_hold 2.0
flow_profile_hold_time 8
flow_profile_decline 1.2
flow_profile_decline_time 17
flow_profile_minimum_pressure 4
preinfusion_flow_rate 4.0
profile_notes {A-Flow: an alternative profile for D-Flow}
final_desired_shot_volume 100
final_desired_shot_weight 36.0
final_desired_shot_weight_advanced 36
tank_desired_water_temperature 0
final_desired_shot_volume_advanced 100
profile_title {A-Flow / default-like-dflow}
profile_language en
preinfusion_stop_pressure 4.0
profile_hide 1
final_desired_shot_volume_advanced_count_start 2
beverage_type espresso
maximum_pressure 0
maximum_pressure_range_advanced 0.6
maximum_flow_range_advanced 0.6
maximum_flow 0
maximum_pressure_range_default 0.9
maximum_flow_range_default 1.0

