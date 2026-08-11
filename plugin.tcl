### By Damian Brakel ###

set ::protected_d_flow_profiles {default {La Pavoni} Q}
set plugin_name "D_Flow_Espresso_Profile"

namespace eval ::plugins::${plugin_name} {
    # These are shown in the plugin selection page
    variable author "Damian Brakel"
    variable contact "via Diaspora"
    variable description "D-Flow is a simple to use advanced profile creating and editing tool"
    variable version 3.1
    variable min_de1app_version {1.36.7}


################# variables
### Info messages
set info_intro {
Select an adjustment dial below to display related information.
A selected dial will be active and adjustable by tapping its arrows.
}

set info_dose {
Dose - means the weight of the beans or the weight of the coffee puck.

The dose weight is used in calcultating the extraction ratio.
See pour stop setting for information of extraction ratio

Being consistant with your grind, dose and puck prep will greatly help you repeatedly extract great espresso.
}

set info_infuse_temp {
Infuse temperature allows you to emulate various machine where the group temperature may start cooler
than the water being added.
See pour temperature for information on how temperature effects extraction and taste.
}

set info_infuse_pressure {
A higher infusion pressure will increase puck resistance

The default is 3 bar, the same as used by LRv2, LRv3 profiles and others, it is tyipcally the upper level used by
machines that use preinfusion.

Some manual lever machines like a LA Pavoni have a boiler pressure of 0.8 to 1 bar, infusing is done by raising, fill
the group with water at boiler pressure. to emulater this you would set a 1.2 bar infuse pressure.
Note: D-Flow allows minimum of 1.2 bar due to prevent unexpected behaviour during high flow

Some machines use pressures anywhere in between. Others may not hold an infusion presure at all, like a
common E61 pump machines, where it apply water straight to extraction pressure, typically 8 or 9 bar.
}

set info_infuse_stop {
Infuse will move on to the pour stage when any one of these settings are reached.
For best consistancy I recommend using weight. In cases where a scale is not available,
you could use volume, however for those situations and where you you may want to end infuse before first drops in the cup,
using time is likely the better option.

A longer infusion increases body, but it also reduces puck resistance during the pour.
Which means we would need to grind finer to maintain the same pour pressure/flow rates.
Finer grinds also reduce body and can increase bitterness, so the aim is to find a ballence for your prefered taste.

A longer infusion also means we have a higher pecentage of the shot being at 3 bar.
0.2g to 4g is a good starting range to experiment with.
}

set info_pour_temp {
Higher temperature helps extract the soluble compounds, however it also increases
harsher tasts that may exist too.

Higher temperatures usually results in lower body with more pronounced acidity, wine, fruity, vegetal flavours.
Lower temperatures often work better with slower extraction rates.
}

set info_pour_limits {
D-Flow uses flow with pressure limits, allows for a greater grind range, giving better control and resulting in less wasted shots.

Increasing pressure will shift taste from wine like to a more syrupy texture, it also
shifts tastes from clear delicate flavours to more muddled flavours.

Faster flows can increase clarity in ligher roasts and may help reduce channeling.
Slower flow can help increase intensity and body in darker than light roasts.
}

set info_pour_stop "

The extraction ratio   (Dose : Extraction)   is shown above the setting dial.
Increasing the extraction ratio will shift the taste from

Sour  >  Sweet  >  Bitter



The ideal extraction ratio can vary between beans, water alkalinity, puck prep methods and how evenly the pack is extracted.
You should adjusted this setting for your taste.
"



if {$::settings(active_settings_tab) == "settings_2c"} {
    set ::settings(active_settings_tab) "settings_1"
}
set settings_tab_font "Helv_10_bold"
set botton_button_font "Helv_10_bold"
set listbox_font "Helv_10"
if {[language] == "ar"} {
	set settings_tab_font "Helv_15_bold"
	set botton_button_font "Helv_15_bold"
} elseif {[language] == "he"} {
	set settings_tab_font "Helv_10_bold"
	set botton_button_font "Helv_12_bold"
} elseif {[language] == "zh-hans" || [language] == "zh-hant" || [language] == "kr"} {
	set settings_tab_font "Helv_15_bold"
	set botton_button_font "Helv_15_bold"
} elseif {[language] != "en" && [language] != "kr" && [language] != "zh-hans" && [language] != "zh-hant"} {
	set settings_tab_font "Helv_8_bold"
}
set page_name {Dflowset Dflowinfo}
set page_set {Dflowset}
set page_info {Dflowinfo}
set font "Roboto-Regular"
set font_colour #7f879a
set icon_colour #7f879a
set info_colour #ff9421
set button_outline_colour #eee
set button_outline_width 2
if {[info exists ::settings(D_Flow_graph_style)] == 0} {
    set ::settings(D_Flow_graph_style) "DSx"
}
if {$::settings(skin) == "DSx"} {
    set ::DSx_settings(orange_cup_indicator) { }
    set ::DSx_settings(blue_cup_indicator) { }
    set ::DSx_settings(pink_cup_indicator) { }
    clear_profile_font
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); back_to_previous_page; fill_advanced_profile_steps_listbox; restore_espresso_chart; LRv2_preview; DSx_graph_restore; save_settings_to_de1; fill_profiles_listbox; fill_extensions_listbox; refresh_DSx_temperature; ::plugins::D_Flow_Espresso_Profile::prep} 1505 1430 2015 1600
    } else {
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); set_next_page off off; page_show off; fill_advanced_profile_steps_listbox; restore_espresso_chart; save_settings_to_de1; fill_profiles_listbox ; fill_extensions_listbox; ::plugins::D_Flow_Espresso_Profile::prep} 1505 1430 2015 1600
}

################ procedures
proc main {} {
}

proc check_Roboto-Regular_exists {} {
    if {[file exists "[homedir]/fonts/Roboto-Regular.ttf"] != 1} {
        catch { file copy -force [homedir]/skins/DSx/DSx_Font_Files/Roboto-Regular.ttf [homedir]/fonts/Roboto-Regular.ttf }
    }
}
check_Roboto-Regular_exists

### Check / write profile
proc set_Dflow_default {} {
    set ::settings(advanced_shot) {{exit_if 1 flow 8 volume 100 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 88 weight 5.00 name Filling pressure 3.0 sensor coffee pump pressure exit_type pressure_over exit_flow_over 6 exit_pressure_over 1.5 max_flow_or_pressure 0 exit_pressure_under 0 seconds 25.00} {exit_if 0 flow 8 volume 100.00 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 88 weight 4.00 name Infusing pressure 3.0 sensor coffee pump pressure exit_type pressure_over exit_flow_over 6 exit_pressure_over 3.0 max_flow_or_pressure 0 exit_pressure_under 0 seconds 60.0} {exit_if 0 flow 1.7 volume 0 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 88 weight 0.00 name Pouring pressure 4.8 sensor coffee pump flow exit_type flow_over exit_flow_over 2.80 exit_pressure_over 11 max_flow_or_pressure 8.5 exit_pressure_under 0 seconds 127}}
    set ::settings(espresso_temperature_steps_enabled) 0
    set ::settings(author) Damian
    set ::settings(espresso_hold_time) 16
    set ::settings(preinfusion_time) 20
    set ::settings(espresso_pressure) 6.0
    set ::settings(espresso_decline_time) 30
    set ::settings(pressure_end) 4.0
    set ::settings(espresso_temperature) 86.0
    set ::settings(espresso_temperature_0) 86.0
    set ::settings(espresso_temperature_1) 86.0
    set ::settings(espresso_temperature_2) 86.0
    set ::settings(espresso_temperature_3) 86.0
    set ::settings(settings_profile_type) settings_2c
    set ::settings(flow_profile_preinfusion) 4
    set ::settings(flow_profile_preinfusion_time) 5
    set ::settings(flow_profile_hold) 2
    set ::settings(flow_profile_hold_time) 8
    set ::settings(flow_profile_decline) 1.2
    set ::settings(flow_profile_decline_time) 17
    set ::settings(flow_profile_minimum_pressure) 4
    set ::settings(preinfusion_flow_rate) 4
    set ::settings(profile_notes) {A simple to use profiling system By Damian Brakel
        D-Flow's Default profile is provided as an example profile for you to get started in creating your own}
    set ::settings(final_desired_shot_volume) 54
    set ::settings(final_desired_shot_weight) 50
    set ::settings(final_desired_shot_weight_advanced) 50
    set ::settings(tank_desired_water_temperature) 0
    set ::settings(final_desired_shot_volume_advanced) 0
    set ::settings(profile_language) en
    set ::settings(preinfusion_stop_pressure) 4.0
    set ::settings(profile_hide) 0
    set ::settings(final_desired_shot_volume_advanced_count_start) 2
    set ::settings(beverage_type) espresso
    set ::settings(maximum_pressure) 0
    set ::settings(maximum_pressure_range_advanced) 0.2
    set ::settings(maximum_flow_range_advanced) 0.2
    set ::settings(maximum_flow) 0
    set ::settings(maximum_pressure_range_default) 0.9
    set ::settings(maximum_flow_range_default) 1.0
    set ::settings(active_settings_tab) "settings_1"
    prep
}

proc prep { args } {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" } {
        array set filling [lindex $::settings(advanced_shot) 0]
        set ::Dflow_filling_temperature $filling(temperature)
        array set soaking [lindex $::settings(advanced_shot) 1]
        set ::Dflow_soaking_seconds [round_to_one_digits $soaking(seconds)]
        set ::Dflow_soaking_pressure $soaking(pressure)
        set ::Dflow_soaking_volume $soaking(volume)
        set ::Dflow_soaking_weight $soaking(weight)
        array set pouring [lindex $::settings(advanced_shot) 2]
        set ::Dflow_pouring_flow [round_to_one_digits $pouring(flow)]
        set ::Dflow_pouring_pressure $pouring(max_flow_or_pressure)
        set ::Dflow_pouring_temperature $pouring(temperature)
    }
}

proc write_La_Pavoni_profile {} {
    set La_Pavoni_data {}
    append La_Pavoni_data {advanced_shot {{exit_if 1 flow 8 volume 60.00 max_flow_or_pressure_range 0.2 transition fast popup {} exit_flow_under 0 temperature 84.0 weight 0.00 name Filling pressure 1.2 sensor coffee pump pressure exit_type pressure_over exit_flow_over 6 exit_pressure_over 1.2 max_flow_or_pressure 0 seconds 25.00 exit_pressure_under 0} {exit_if 0 flow 8 volume 100.00 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 94.0 weight 0.2 name Infusing pressure 1.2 pump pressure sensor coffee exit_type pressure_over exit_flow_over 6 exit_pressure_over 3.0 max_flow_or_pressure 0 exit_pressure_under 0 seconds 60.0} {exit_if 0 flow 2.4 volume 100 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 94.0 weight 0.00 name Pouring pressure 4.8 pump flow sensor coffee exit_type flow_over exit_flow_over 2.80 exit_pressure_over 11 max_flow_or_pressure 9.0 exit_pressure_under 0 seconds 127}}
espresso_temperature_steps_enabled 0
author Damian
read_only 1
read_only_backup {}
espresso_hold_time 16
preinfusion_time 20
espresso_pressure 6.0
espresso_decline_time 30
pressure_end 4.0
espresso_temperature 84.0
espresso_temperature_0 84.0
espresso_temperature_1 84.0
espresso_temperature_2 84.0
espresso_temperature_3 84.0
settings_profile_type settings_2c
flow_profile_preinfusion 4
flow_profile_preinfusion_time 5
flow_profile_hold 2
flow_profile_hold_time 8
flow_profile_decline 1.2
flow_profile_decline_time 17
flow_profile_minimum_pressure 4
preinfusion_flow_rate 4
profile_notes {}
final_desired_shot_volume 36
final_desired_shot_weight 57.0
final_desired_shot_weight_advanced 46
tank_desired_water_temperature 0
final_desired_shot_volume_advanced 0
profile_title {D-Flow / La Pavoni}
profile_language en
preinfusion_stop_pressure 4.0
profile_hide 0
final_desired_shot_volume_advanced_count_start 2
beverage_type espresso
maximum_pressure 0
maximum_pressure_range_advanced 0.2
maximum_flow_range_advanced 0.2
maximum_flow 0
maximum_pressure_range_default 0.9
maximum_flow_range_default 1.0}
    write_file [data_directory]/profiles/D-Flow____La_Pavoni.tcl $La_Pavoni_data
}

proc write_Q_profile {} {
    set Q_data {}
    append Q_data {advanced_shot {{exit_if 1 flow 8 volume 60.00 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 84.0 weight 0.00 name Filling pressure 6.0 pump pressure sensor coffee exit_type pressure_over exit_flow_over 6 max_flow_or_pressure 0 exit_pressure_over 3.0 seconds 25.00 exit_pressure_under 0} {exit_if 0 flow 8 volume 100.00 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 94.0 weight 0.0 name Infusing pressure 6.0 pump pressure sensor coffee exit_type pressure_over exit_flow_over 6 max_flow_or_pressure 0 exit_pressure_over 3.0 seconds 1.0 exit_pressure_under 0} {exit_if 0 flow 1.8 volume 100 max_flow_or_pressure_range 0.2 transition fast exit_flow_under 0 temperature 94.0 weight 0.00 name Pouring pressure 4.8 pump flow sensor coffee exit_type flow_over exit_flow_over 2.80 max_flow_or_pressure 10.0 exit_pressure_over 11 seconds 127 exit_pressure_under 0}}
espresso_temperature_steps_enabled 0
author Damian
read_only 1
read_only_backup {}
espresso_hold_time 16
preinfusion_time 20
espresso_pressure 6.0
espresso_decline_time 30
pressure_end 4.0
espresso_temperature 84.0
espresso_temperature_0 84.0
espresso_temperature_1 84.0
espresso_temperature_2 84.0
espresso_temperature_3 84.0
settings_profile_type settings_2c
flow_profile_preinfusion 4
flow_profile_preinfusion_time 5
flow_profile_hold 2
flow_profile_hold_time 8
flow_profile_decline 1.2
flow_profile_decline_time 17
flow_profile_minimum_pressure 4
preinfusion_flow_rate 8
profile_notes {}
final_desired_shot_volume 0
final_desired_shot_weight 36
final_desired_shot_weight_advanced 36
tank_desired_water_temperature 0
final_desired_shot_volume_advanced 0
profile_title {D-Flow / Q}
profile_language en
preinfusion_stop_pressure 4.0
profile_hide 0
final_desired_shot_volume_advanced_count_start 2
beverage_type espresso
maximum_pressure 0
maximum_pressure_range_advanced 0.2
maximum_flow_range_advanced 0.2
maximum_flow 0
maximum_pressure_range_default 0.9
maximum_flow_range_default 1.0}
    write_file [data_directory]/profiles/D-Flow____Q.tcl $Q_data
}

if {[info exists ::settings(D_Flow_update)] == 0} {
    set ::settings(D_Flow_update) 0
}

if { $::settings(D_Flow_update) < 2 || \
    [file exists "[data_directory]/profiles/D-Flow____default.tcl"] != 1 || \
    [file exists "[data_directory]/profiles/D-Flow____Q.tcl"] != 1 || \
    [file exists "[data_directory]/profiles/D-Flow____La_Pavoni.tcl"] != 1  } {
    set ::settings(profile_title) {D-Flow / default}
    set_Dflow_default
    set ::settings(original_profile_title) $::settings(profile_title)
    set ::settings(profile_filename) "D-Flow____default"
    set ::settings(profile_to_save) $::settings(profile_title)
    set ::settings(D_Flow_update) 2
    save_profile
    write_Q_profile
    write_La_Pavoni_profile
}

if {[file exists "[data_directory]/profiles/D-Flow____Q.tcl"] != 1 } {
    write_Q_profile
    write_La_Pavoni_profile
}


prep

proc update_D-Flow {} {
    set ::settings(espresso_temperature) $::Dflow_filling_temperature
    array set filling [lindex $::settings(advanced_shot) 0]
    array set soaking [lindex $::settings(advanced_shot) 1]
    array set pouring [lindex $::settings(advanced_shot) 2]
    set filling(temperature) $::Dflow_filling_temperature
    set filling(pressure) $::Dflow_soaking_pressure
    if {$::Dflow_soaking_pressure < 2.8} {
        set filling(exit_pressure_over) $::Dflow_soaking_pressure
    } else {
        set filling(exit_pressure_over) [round_to_one_digits [expr {($::Dflow_soaking_pressure / 2) + 0.6}]]
    }
    if {$filling(exit_pressure_over) < 1.2} {set filling(exit_pressure_over) 1.2}
    set soaking(temperature) $::Dflow_pouring_temperature
    set soaking(pressure) $::Dflow_soaking_pressure
    set soaking(seconds) $::Dflow_soaking_seconds
    set soaking(volume) $::Dflow_soaking_volume
    set soaking(weight) $::Dflow_soaking_weight
    set pouring(temperature) $::Dflow_pouring_temperature
    set pouring(flow) $::Dflow_pouring_flow
    set pouring(max_flow_or_pressure) $::Dflow_pouring_pressure
    set newprofile {}
    lappend newprofile [array get filling]
    lappend newprofile [array get soaking]
    lappend newprofile [array get pouring]
    set ::settings(advanced_shot) $newprofile
    range_check_shot_variables
    profile_has_changed_set
    ::plugins::D_Flow_Espresso_Profile::demo_graph
}

proc format_seconds {n} {
	set num [round_to_integer $n]
	if {$num == 0} {
		set output [translate "off"]
	} elseif {$num == 1} {
		set output [subst {$num [translate "sec"]}]
	} elseif {$num == 60} {
		set output [translate "1 min"]
	} elseif {$num > 60} {
		set m [round_to_integer [expr {$num / 60}]]
		set s [round_to_integer [expr {$num - ($m * 60)}]]
		set min [translate "m"]
		set sec [translate "s"]
		set output $m$min$s$sec
	} else {
		set output [subst {$num [translate "sec"]}]
	}
	return $output
}

proc format_SAW {n} {
	if {$n == 0 || $n == ""} {
		return [translate "off"]
	} else {
	    if {$::de1(language_rtl) == 1} {
			return [subst {[translate "g"][round_to_one_digits $n]}]
		}
		if {$::settings(enable_fluid_ounces) != 1} {
			return [subst {[round_to_one_digits $n][translate "g"]}]
		} else {
			return [subst {[round_to_one_digits [ml_to_oz $n]] oz}]
		}
	}
}

proc extraction_ratio {} {
    if {$::settings(final_desired_shot_volume_advanced) > 0 && $::settings(final_desired_shot_volume_advanced) < $::settings(final_desired_shot_weight_advanced)} {
	    set b "1 : "
        set r [round_to_one_digits [expr (0.01 + $::settings(final_desired_shot_volume_advanced))/$::settings(grinder_dose_weight)]]
        return $b$r
	} else {
	    set b "1 : "
        set r [round_to_one_digits [expr (0.01 + $::settings(final_desired_shot_weight_advanced))/$::settings(grinder_dose_weight)]]
        return $b$r
	}

}

proc format_weight_measurement {n} {
	if {$n == 0 || $n == ""} {
		return [translate "off"]
	} else {
	    if {$::de1(language_rtl) == 1} {
			return [subst {[translate "g"][round_to_integer $n]}]
		}
		if {$::settings(enable_fluid_ounces) != 1} {
			return [subst {[round_to_integer $n][translate "g"]}]
		} else {
			return [subst {[round_to_one_digits [ml_to_oz $n]] oz}]
		}
	}
}

proc save_D-Flow_profile {} {
    set pre "D-Flow____"
    set df "D-Flow / "
    set profile_filename $pre$::DFlow_name
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {[file exists "[data_directory]/profiles/${profile_filename}.tcl"] != 1} {
        if {$title_test == "D-Flow /" } {
            set ::settings(profile_title) $df$::DFlow_name;
        } else {
            set ::settings(profile_title) $::DFlow_name;
        }
        borg toast [translate "Saved"]
        save_profile
        ::plugins::D_Flow_Espresso_Profile::demo_graph

    } else {
        set ::Dflow_message "$df$::DFlow_name [translate "already exists"]"
        after 1200 {set ::Dflow_message ""}
    }

}

proc tap_to_update {} {
    if {$::settings(profile_has_changed) == 1} {
        return "*tap here to save changes*"
    } else {
        return ""
    }
}

proc D-Flow_data {} {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" } {
        set a [round_to_integer $::Dflow_filling_temperature]
        set b [return_temperature_setting $::Dflow_pouring_temperature]
        set c [::plugins::D_Flow_Espresso_Profile::format_SAW $::Dflow_soaking_weight]
        set d [return_flow_measurement $::Dflow_pouring_flow]
        set f [return_pressure_measurement $::Dflow_pouring_pressure]
        set s {  }
        set m {-}
        return $a$m$b$s$s$c$s$s$d$s$f
    }
}

proc toggle_graph {} {
    if {$::settings(D_Flow_graph_style) == "Insight"} {
        set ::settings(D_Flow_graph_style) "DSx"
    } else {
        set ::settings(D_Flow_graph_style) "Insight"
    }
    ::plugins::D_Flow_Espresso_Profile::select_flow_curve
}
proc select_flow_curve {} {
    if {$::settings(D_Flow_graph_style) == "Insight"} {
        $::Dflow_demo_graph element configure line_espresso_de1_explanation_chart_flow -ydata espresso_de1_explanation_chart_flow
        $::Dflow_demo_graph axis configure x -color #5a5d75
        $::Dflow_demo_graph axis configure y -color #5a5d75
        $::Dflow_demo_graph axis configure y2 -hide 1
        $::Dflow_demo_graph grid configure -color #ddd
        $::Dflow_demo_graph configure -plotbackground #f8f8f8 -background #fff -plotrelief raised
        dui item moveto Dflowset inpor 2240 270
    } else {
        $::Dflow_demo_graph element configure line_espresso_de1_explanation_chart_flow -ydata espresso_de1_explanation_chart_flow_2x
        $::Dflow_demo_graph axis configure x -color #5a5d75
        $::Dflow_demo_graph axis configure y -color #18c37e
        $::Dflow_demo_graph axis configure y2 -hide 0
        $::Dflow_demo_graph grid configure -color #ddd
        $::Dflow_demo_graph configure -plotbackground #f8f8f8 -background #fff -plotrelief raised
        dui item moveto Dflowset inpor 2186 270

    }
}

proc demo_graph { {context {}} } {
	::plugins::D_Flow_Espresso_Profile::prep
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" } {
        espresso_de1_explanation_chart_elapsed length 0
        espresso_de1_explanation_chart_temperature length 0
        espresso_de1_explanation_chart_temperature_10 length 0
        espresso_de1_explanation_chart_selected_step length 0
        espresso_de1_explanation_chart_pressure length 0
        espresso_de1_explanation_chart_flow length 0
        espresso_de1_explanation_chart_elapsed_flow length 0
        espresso_de1_explanation_chart_flow_2x length 0
        espresso_de1_explanation_chart_pressure append 0
        espresso_de1_explanation_chart_flow append 0
        espresso_de1_explanation_chart_elapsed append 0
        espresso_de1_explanation_chart_elapsed_flow append 0
        array set props [lindex $::settings(advanced_shot) 0]
        set c 0
        while {$c < 11} {
            incr c
            espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
            espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
        }
        set sp $::Dflow_soaking_pressure
        set sp_b [expr {$sp*0.93}]
        set sp_a [expr {$sp*0.7}]
        espresso_de1_explanation_chart_pressure append {0.0 0.0 0.0 0.0 $sp_a $sp_b $sp $sp $sp $sp $sp}
        #espresso_de1_explanation_chart_pressure append {0.0 0.0 0.0 0.0 2.1 2.8 3.0 3.0 3.0 3.0 3.0}
        espresso_de1_explanation_chart_flow append {0.0 5.6 7.6 8.3 8.2 6.0 2.9 1.3 0.6 0.4 0.3}
        espresso_de1_explanation_chart_elapsed append {0.008 0.994 2.03 3.015 4.004 4.994 6.036 7.03 8.017 8.999 15}
        espresso_de1_explanation_chart_elapsed_flow append {0.008 0.994 2.03 3.015 4.004 4.994 6.036 7.03 8.017 8.999 15}
        array set props [lindex $::settings(advanced_shot) 2]
        if {$::settings(final_desired_shot_volume_advanced) > 0 && $::settings(final_desired_shot_volume_advanced) < $::settings(final_desired_shot_weight_advanced)} {
            set shotendtime [expr {$::settings(final_desired_shot_volume_advanced) / [ifexists props(flow)] + 16}]
        } else {
            set shotendtime [expr {$::settings(final_desired_shot_weight_advanced) / [ifexists props(flow)] + 16}]
        }
        espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
        espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
        espresso_de1_explanation_chart_pressure append [ifexists props(max_flow_or_pressure)]
        espresso_de1_explanation_chart_flow append [ifexists props(flow)]
        espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
        espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
        espresso_de1_explanation_chart_elapsed append 16
        espresso_de1_explanation_chart_elapsed_flow append 16
        espresso_de1_explanation_chart_pressure append [ifexists props(max_flow_or_pressure)]
        espresso_de1_explanation_chart_flow append [ifexists props(flow)]
        espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
        espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
        espresso_de1_explanation_chart_elapsed append $shotendtime
        espresso_de1_explanation_chart_elapsed_flow append $shotendtime
        foreach f [espresso_de1_explanation_chart_flow range 0 end] {
            espresso_de1_explanation_chart_flow_2x append [expr {2.0 * $f}]
        }
        dui item moveto Dflowset inpoc [expr {1130 + (1120 * 15 / (0.01 + $shotendtime))}] 270
        dui item moveto Dflowset inpoi [expr {1140 + ((1160 + (1120 * 15 / (0.01 + $shotendtime)) - 1170) / 2)}] 270
        if {$::settings(D_Flow_graph_style) == "Insight"} {
            set xcord 2240
        } else {
            set xcord 2186
        }
        dui item moveto Dflowset inpop [expr {$xcord - (($xcord - (1130 + (1120 * 15 / (0.01 + $shotendtime)))) / 2)}] 270

	}
}

proc reset_button_canvas {} {
    dui item config Dflowinfo dose_bg -outline #e9e9ed
    dui item config Dflowinfo infuse_temp_bg -outline #e9e9ed
    dui item config Dflowinfo infuse_pressure_bg -outline #e9e9ed
    dui item config Dflowinfo infuse_stop_bg -outline #e9e9ed
    dui item config Dflowinfo pour_temp_bg -outline #e9e9ed
    dui item config Dflowinfo pour_limits_bg -outline #e9e9ed
    dui item config Dflowinfo pour_stop_bg -outline #e9e9ed

    dui item config Dflowinfo info_intro -state hidden
    dui item config Dflowinfo info_dose -state hidden
    dui item config Dflowinfo info_infuse_temp -state hidden
    dui item config Dflowinfo info_infuse_pressure -state hidden
    dui item config Dflowinfo info_infuse_stop -state hidden
    dui item config Dflowinfo info_pour_temp -state hidden
    dui item config Dflowinfo info_pour_limits -state hidden
    dui item config Dflowinfo info_pour_stop -state hidden

    dui item show Dflowinfo dose_info_button*
    dui item show Dflowinfo infuse_temp_info_button*
    dui item show Dflowinfo infuse_pressure_info_button*
    dui item show Dflowinfo infuse_stop_info_button*
    dui item show Dflowinfo pour_temp_info_button*
    dui item show Dflowinfo pour_limits_info_button*
    dui item show Dflowinfo pour_stop_info_button*

    set ::plugins::D_Flow_Espresso_Profile::info " "
}

################ page
### background
add_de1_page $page_name "settings_2c2.png" "default"
dui add canvas_item rect $page_name 0 1424 1300 1600 -fill "#d7d9e6" -width 0
dui add canvas_item rect $page_name 14 940 2546 1420 -fill #ededfa -width 0
dui add canvas_item rect $page_info 1400 1450 2546 1600 -fill #d7d9e6 -width 0

dui add canvas_item rect $page_name 57 960 283 1046 -fill #fff -width 2 -outline #e9e9ed
dui add canvas_item rect $page_name 317 960 1393 1046 -fill #fff -width 2 -outline #e9e9ed
dui add canvas_item rect $page_name 1427 960 2493 1046 -fill #fff -width 2 -outline #e9e9ed

dui add canvas_item rect $page_name 57 1050 283 1400 -fill #fff -width 2 -outline #e9e9ed -tags dose_bg
dui add canvas_item rect $page_name 317 1050 543 1400 -fill #fff -width 2 -outline #e9e9ed -tags infuse_temp_bg
dui add canvas_item rect $page_name 547 1050 793 1400 -fill #fff -width 2 -outline #e9e9ed -tags infuse_pressure_bg
dui add canvas_item rect $page_name 797 1050 1395 1400 -fill #fff -width 2 -outline #e9e9ed -tags infuse_stop_bg
dui add canvas_item rect $page_name 1427 1050 1653 1400 -fill #fff -width 2 -outline #e9e9ed -tags pour_temp_bg
dui add canvas_item rect $page_name 1657 1050 2083 1400 -fill #fff -width 2 -outline #e9e9ed -tags pour_limits_bg
dui add canvas_item rect $page_name 2087 1050 2493 1400 -fill #fff -width 2 -outline #e9e9ed -tags pour_stop_bg

dui add canvas_item rect $page_name 920 1386 1260 1436 -fill #ededfa -width 0 -outline #e9e9ed
dui add canvas_item rect $page_name 1720 1386 2020 1436 -fill #ededfa -width 0 -outline #e9e9ed
dui add canvas_item rect $page_name 2190 1386 2390 1436 -fill #ededfa -width 0 -outline #e9e9ed

### Settings
dui add variable $page_name 400 490 -justify center -anchor center -font [dui font get $font 16] -fill #ff574a -textvariable {$::Dflow_message}
dui add variable $page_name 1000 170 -justify center -anchor center -font [dui font get $font 12] -fill #ff9421 -textvariable {[::plugins::D_Flow_Espresso_Profile::tap_to_update]}

dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -tags info_intro -text $::plugins::D_Flow_Espresso_Profile::info_intro
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_dose -text $::plugins::D_Flow_Espresso_Profile::info_dose
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_temp -text $::plugins::D_Flow_Espresso_Profile::info_infuse_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_pressure -text $::plugins::D_Flow_Espresso_Profile::info_infuse_pressure
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_stop -text $::plugins::D_Flow_Espresso_Profile::info_infuse_stop
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_temp -text $::plugins::D_Flow_Espresso_Profile::info_pour_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_limits -text $::plugins::D_Flow_Espresso_Profile::info_pour_limits
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_stop -text $::plugins::D_Flow_Espresso_Profile::info_pour_stop
dui add dtext $page_info 1280 900 -justify center -anchor center -font [dui font get $font 16] -fill #d7d9e6 -text {Tap within this info window to exit}

dui add dtext $page_name 1092 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text {infuse until either}
dui add dtext $page_name 1850 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text {maximum limits}
dui add dtext $page_name 2290 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text {stop setting}

dui add dtext $page_name 170 1010 -justify center -anchor center -font [dui font get $font 20] -fill #d7d9e6 -text {D O S E}
dui add dtext $page_name 170 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {weight}
dui add variable $page_name 170 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_weight_measurement $::settings(grinder_dose_weight)]}

dui add dtext $page_name 825 1010 -justify center -anchor center -font [dui font get $font 24] -fill #d7d9e6 -text {I N F U S E}
dui add dtext $page_name 430 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {temperature}
dui add variable $page_name 430 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_temperature_setting $::Dflow_filling_temperature]}
dui add dtext $page_name 670 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {pressure}
dui add variable $page_name 680 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_pressure_measurement $::Dflow_soaking_pressure]}
dui add dtext $page_name 910 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {time}
dui add dtext $page_name 1090 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {volume}
dui add dtext $page_name 1270 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {weight}
dui add variable $page_name 910 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::D_Flow_Espresso_Profile::format_seconds $::Dflow_soaking_seconds]}
dui add variable $page_name 1090 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_stop_at_volume_measurement $::Dflow_soaking_volume]}
dui add variable $page_name 1270 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::D_Flow_Espresso_Profile::format_SAW $::Dflow_soaking_weight]}

dui add dtext $page_name 1960 1010 -justify center -anchor center -font [dui font get $font 24] -fill #d7d9e6 -text {P O U R}
dui add variable $page_name 2290 1010 -justify center -anchor center -font [dui font get $font 20] -fill #b7b9c6 -textvariable {[::plugins::D_Flow_Espresso_Profile::extraction_ratio]}
dui add dtext $page_name 1540 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {temperature}
dui add dtext $page_name 1780 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {flow}
dui add dtext $page_name 1960 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {pressure}
dui add variable $page_name 1540 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_temperature_setting $::Dflow_pouring_temperature]}
dui add variable $page_name 1780 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_flow_measurement $::Dflow_pouring_flow]}
dui add variable $page_name 1970 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_pressure_measurement $::Dflow_pouring_pressure]}
dui add dtext $page_name 2200 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {volume}
dui add dtext $page_name 2380 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text {weight}
dui add variable $page_name 2200 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_stop_at_volume_measurement $::settings(final_desired_shot_volume_advanced)]}
dui add variable $page_name 2380 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::D_Flow_Espresso_Profile::format_weight_measurement $::settings(final_desired_shot_weight_advanced)]}
################################
# Bean weight
dui add dbutton $page_name 100 1050 \
    -bwidth 150 -bheight 200 \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(grinder_dose_weight) [round_to_one_digits [expr {$::settings(grinder_dose_weight) + 0.1}]]
        set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    }
dui add dbutton $page_name 100 1250 \
    -bwidth 150 -bheight 200 \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(grinder_dose_weight) [round_to_one_digits [expr {$::settings(grinder_dose_weight) - 0.1}]]
        if {$::settings(grinder_dose_weight) < 0} {set ::settings(grinder_dose_weight) 0}
        set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    }

dui add dbutton $page_name 100 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::settings(grinder_dose_weight) -n_decimals 1 -min 0 -max 40 -default $::settings(grinder_dose_weight) -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Dose weight"] -previous_values [::dui::pages::dui_number_editor::get_previous_values dose] -return_callback "dflow_callback3 callback_after_adv_profile_data_entry dose"
    }

# Fill Temperature
dui add dbutton $page_name 340 1050 \
    -bwidth 180 -bheight 200 -tags fill_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_filling_temperature [round_to_integer [expr {$::Dflow_filling_temperature + 1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 340 1250 \
    -bwidth 180 -bheight 200 -tags fill_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_filling_temperature [round_to_integer [expr {$::Dflow_filling_temperature - 1}]]
        if {$::Dflow_filling_temperature < 80} {set ::Dflow_filling_temperature 80}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 340 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_filling_temperature -n_decimals 0 -min 80 -max 105 -default $::Dflow_filling_temperature -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Infuse Temperature"] -previous_values [::dui::pages::dui_number_editor::get_previous_values temp] -return_callback "dflow_callback callback_after_adv_profile_data_entry temp"
    }

# Soaking flow pressure
dui add dbutton $page_name 580 1050 \
    -bwidth 180 -bheight 200 -tags soak_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_pressure [round_to_one_digits [expr {$::Dflow_soaking_pressure + 0.1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 580 1250 \
    -bwidth 180 -bheight 200 -tags soak_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_pressure [round_to_one_digits [expr {$::Dflow_soaking_pressure - 0.1}]]
        if {$::Dflow_soaking_pressure < 1.2} {set ::Dflow_soaking_pressure 1.2}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 580 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_soaking_pressure -n_decimals 1 -min 1.2 -max 12 -default $::Dflow_soaking_pressure -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Infuse Pressure"] -previous_values [::dui::pages::dui_number_editor::get_previous_values pressure] -return_callback "dflow_callback callback_after_adv_profile_data_entry pressure"
    }

# Move on buttons
dui add dbutton $page_name 820 1050 \
    -bwidth 180 -bheight 200 -tags soak_seconds_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_seconds [round_to_integer [expr {$::Dflow_soaking_seconds + 1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 820 1250 \
    -bwidth 180 -bheight 200 -tags soak_seconds_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_seconds [round_to_integer [expr {$::Dflow_soaking_seconds - 1}]]
        if {$::Dflow_soaking_seconds < 0} {set :::Dflow_soaking_seconds 0}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 820 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_soaking_seconds -n_decimals 0 -min 1 -max 127 -default $::Dflow_soaking_seconds -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Infuse Time (seconds)"] -previous_values [::dui::pages::dui_number_editor::get_previous_values time] -return_callback "dflow_callback callback_after_adv_profile_data_entry time"
    }

dui add dbutton $page_name 1000 1050 \
    -bwidth 180 -bheight 200 -tags soak_volume_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_volume [round_to_integer [expr {$::Dflow_soaking_volume + 1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1000 1250 \
    -bwidth 180 -bheight 200 -tags soak_volume_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_volume [round_to_integer [expr {$::Dflow_soaking_volume - 1}]]
        if {$::Dflow_soaking_volume < 0} {set ::Dflow_soaking_volume 0}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1000 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_soaking_volume -n_decimals 0 -min 50 -max 127 -default $::Dflow_soaking_volume -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Infuse Volume"] -previous_values [::dui::pages::dui_number_editor::get_previous_values volume] -return_callback "dflow_callback callback_after_adv_profile_data_entry volume"
    }

dui add dbutton $page_name 1180 1050 \
    -bwidth 180 -bheight 200 -tags soak_weight_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_weight [round_to_one_digits [expr {$::Dflow_soaking_weight + 0.2}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1180 1250 \
    -bwidth 180 -bheight 200 -tags soak_weight_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_soaking_weight [round_to_one_digits [expr {$::Dflow_soaking_weight - 0.2}]]
        if {$::Dflow_soaking_weight < 0} {set ::Dflow_soaking_weight 0}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1180 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_soaking_weight -n_decimals 1 -min 0 -max 500 -default $::Dflow_soaking_weight -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Infuse Weight"] -previous_values [::dui::pages::dui_number_editor::get_previous_values weight] -return_callback "dflow_callback callback_after_adv_profile_data_entry weight"
    }

# pour buttons
dui add dbutton $page_name 1450 1050 \
    -bwidth 180 -bheight 200 -tags pour_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_temperature [round_to_integer [expr {$::Dflow_pouring_temperature + 1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1450 1250 \
    -bwidth 180 -bheight 200 -tags pour_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_temperature [round_to_integer [expr {$::Dflow_pouring_temperature - 1}]]
        if {$::Dflow_pouring_temperature < 0} {set ::Dflow_pouring_temperature 0}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1450 1210 \
    -bwidth 1450 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_pouring_temperature -n_decimals 0 -min 80 -max 105 -default $::Dflow_pouring_temperature -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Pour Temperature"] -previous_values [::dui::pages::dui_number_editor::get_previous_values temp] -return_callback "dflow_callback callback_after_adv_profile_data_entry temp"
    }

dui add dbutton $page_name 1690 1050 \
    -bwidth 180 -bheight 200 -tags pouring_flow_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_flow [round_to_one_digits [expr {$::Dflow_pouring_flow + 0.1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1690 1250 \
    -bwidth 180 -bheight 200 -tags pouring_flow_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_flow [round_to_one_digits [expr {$::Dflow_pouring_flow - 0.1}]]
        if {$::Dflow_pouring_flow < 0.1} {set ::Dflow_pouring_flow 0.1}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1690 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_pouring_flow -n_decimals 1 -min 0.1 -max 8 -default $::Dflow_pouring_flow -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Pour Flow rate"] -previous_values [::dui::pages::dui_number_editor::get_previous_values flow] -return_callback "dflow_callback callback_after_adv_profile_data_entry flow"
    }

dui add dbutton $page_name 1870 1050 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_pressure [round_to_one_digits [expr {$::Dflow_pouring_pressure + 0.1}]]
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1870 1250 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Dflow_pouring_pressure [round_to_one_digits [expr {$::Dflow_pouring_pressure - 0.1}]]
        if {$::Dflow_pouring_pressure < 0} {set ::Dflow_pouring_pressure 0}
        ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    }
dui add dbutton $page_name 1870 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Dflow_pouring_pressure -n_decimals 1 -min 1 -max 12 -default $::Dflow_pouring_pressure -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Pour Pressure"] -previous_values [::dui::pages::dui_number_editor::get_previous_values pressure] -return_callback "dflow_callback callback_after_adv_profile_data_entry pressure"
    }

# stop
dui add dbutton $page_name 2110 1050 \
    -bwidth 180 -bheight 200 -tags SAV_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_volume_advanced) [round_to_integer [expr {$::settings(final_desired_shot_volume_advanced) + 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    }
dui add dbutton $page_name 2110 1250 \
    -bwidth 180 -bheight 200 -tags SAV_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_volume_advanced) [round_to_integer [expr {$::settings(final_desired_shot_volume_advanced) - 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    }
dui add dbutton $page_name 2110 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::settings(final_desired_shot_volume_advanced) -n_decimals 0 -min 0 -max 1000 -default $::settings(final_desired_shot_volume_advanced) -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Pour Volume"] -previous_values [::dui::pages::dui_number_editor::get_previous_values sav] -return_callback "dflow_callback2 callback_after_adv_profile_data_entry sav"
    }

dui add dbutton $page_name 2290 1050 \
    -bwidth 180 -bheight 200 -tags SAW_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) + 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    }
dui add dbutton $page_name 2290 1250 \
    -bwidth 180 -bheight 200 -tags SAW_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) - 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    }
dui add dbutton $page_name 2290 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::settings(final_desired_shot_weight_advanced) -n_decimals 0 -min 0 -max 1000 -default $::settings(final_desired_shot_weight_advanced) -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Pour Weight"] -previous_values [::dui::pages::dui_number_editor::get_previous_values saw] -return_callback "dflow_callback2 callback_after_adv_profile_data_entry saw"
    }
proc ::dflow_callback {nextproc context data} {
    ::plugins::D_Flow_Espresso_Profile::update_D-Flow
    ::dui::pages::dui_number_editor::save_previous_value $nextproc $context $data
}

proc ::dflow_callback2 {nextproc context data} {
    range_check_shot_variables
    profile_has_changed_set
    ::plugins::D_Flow_Espresso_Profile::demo_graph
    ::dui::pages::dui_number_editor::save_previous_value $nextproc $context $data
}

proc ::dflow_callback3 {nextproc context data} {
    set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    ::dui::pages::dui_number_editor::save_previous_value $nextproc $context $data
}
### reset changes
dui add dbutton $page_set 270 300 \
    -bwidth 210 -bheight 160 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "reset\rchanges" -label_font [dui font get $font 14] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        if {[ifexists ::profiles_hide_mode] == 1} {
            unset -nocomplain ::profiles_hide_mode
            fill_profiles_listbox
            }
            array unset ::settings {\*}
            array set ::settings [array get ::settings_backup]
            update_de1_explanation_chart
            fill_skin_listbox
            profile_has_changed_set_colors
            say [translate {Cancel}] $::settings(sound_button_in)
            fill_advanced_profile_steps_listbox
            restore_espresso_chart
            save_settings_to_de1
            fill_profiles_listbox
            fill_extensions_listbox
            #::plugins::D_Flow_Espresso_Profile::update_D-Flow
            ::plugins::D_Flow_Espresso_Profile::demo_graph
        }

### set defaults
set {} {
dui add dbutton $page_set 500 300 \
    -bwidth 210 -bheight 160 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "load default\rvalues" -label_font [dui font get $font 14] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        ::plugins::D_Flow_Espresso_Profile::set_Dflow_default
        ::plugins::D_Flow_Espresso_Profile::demo_graph
        set ::settings(profile_has_changed) 1
    }
}

### Save as
dui add dbutton $page_set 85 555 \
    -bwidth 630 -bheight 230 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -command {
        # do nothing to avoid warning
    }

dui add dbutton $page_set 100 570 \
    -bwidth 600 -bheight 100 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "save as" -label_font [dui font get $font 16] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        say [translate {save}] $::settings(sound_button_in)
        ::plugins::D_Flow_Espresso_Profile::save_D-Flow_profile
    }

dui add dtext $page_set 110 700 -justify center -anchor nw -font [dui font get $font 16] -fill $font_colour -text {D-Flow / }
add_de1_widget $page_set entry 270 690  {
    set ::globals(widget_dflow_save_as) $widget
    bind $widget <Return> { say [translate {save}] $::settings(sound_button_in)
    ::plugins::D_Flow_Espresso_Profile::save_D-Flow_profile; hide_android_keyboard}
    bind $widget <Leave> hide_android_keyboard
} -width 18 -font Helv_8  -borderwidth 1 -bg #fbfaff  -foreground #4e85f4 -textvariable ::DFlow_name -relief flat  -highlightthickness 1 -highlightcolor #000000

### Graph
dui add dtext $page_set 1140 270  -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {| <}
dui add dtext $page_set 2240 270 -tags inpor -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> |}
dui add dtext $page_set 1300 270 -tags inpoi -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Infuse}
dui add dtext $page_set 1840 270 -tags inpop -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Pour}
dui add dtext $page_set 1470 270 -tags inpoc -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> | <}

add_de1_widget "Dflowset" graph 1030 290 {
    set ::Dflow_demo_graph $widget
    ::plugins::D_Flow_Espresso_Profile::demo_graph;

		$widget element create line_espresso_de1_explanation_chart_pressure -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_flow -xdata espresso_de1_explanation_chart_elapsed_flow -ydata espresso_de1_explanation_chart_flow  -label "" -linewidth [rescale_x_skin 12] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_temp -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_temperature_10  -label "" -linewidth [rescale_x_skin 10] -color #ff888c  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;


    $widget axis configure x -color #ddd -tickfont Helv_7 -min 0.0;
    $widget axis configure y -color #18c37e -tickfont Helv_7 -min 0.0 -max $::de1(max_pressure) -subdivisions 5 -majorticks {0  2  4  6  8  10  12}  -hide 0;
    $widget axis configure y2 -color #4e85f4 -tickfont Helv_7 -min 0.0 -max 6 -subdivisions 2 -majorticks {0  1  2  3  4  5  6} -hide 1;
    $widget grid configure -color #555
} -plotbackground #1e1e1e -width [rescale_x_skin 1250] -height [rescale_y_skin 590] -borderwidth 1 -background #1e1e1e -plotrelief flat
::plugins::D_Flow_Espresso_Profile::select_flow_curve

dui add dbutton Dflowset 2300 400 \
    -bwidth 200 -bheight 200 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label {showing} -label_font [dui font get $font 12] -label_justify center -label_anchor center -label_fill $font_colour -label_pos {0.5 0.28} \
    -label1variable {$::settings(D_Flow_graph_style)} -label1_justify center -label1_anchor center -label1_font [dui font get $font 16] -label1_fill $font_colour -label1_pos {0.5 0.5} \
    -label2 {style} -label2_font [dui font get $font 12] -label2_justify center -label2_anchor center -label2_fill $font_colour -label2_pos {0.5 0.72} \
    -command {
        ::plugins::D_Flow_Espresso_Profile::toggle_graph
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    }

### Version credit
dui add variable $page_name 40 1570 -justify left -anchor w -font [dui font get "Font Awesome 5 Pro-Regular-400" 14] -fill #bbb -textvariable {D-Flow $::plugins::D_Flow_Espresso_Profile::version - by $::plugins::D_Flow_Espresso_Profile::author}

#### Info Page
### Info button
dui add dbutton $page_set 90 300 \
    -bwidth 160 -bheight 160 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "\uf129" -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 20] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        page_show Dflowinfo
    }
dui add dbutton $page_info 30 220 \
    -bwidth 2500 -bheight 710 \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        page_show Dflowset
    }

#####
dui add dbutton $page_info 55 1050 \
    -bwidth 230 -bheight 350 -tags dose_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_dose -state normal
        dui item config Dflowinfo dose_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo dose_info_button*

    }
dui add dbutton $page_info 315 1050 \
    -bwidth 230 -bheight 350 -tags infuse_temp_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_infuse_temp -state normal
        dui item config Dflowinfo infuse_temp_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo infuse_temp_info_button*
    }
dui add dbutton $page_info 545 1050 \
    -bwidth 230 -bheight 350 -tags infuse_pressure_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_infuse_pressure -state normal
        dui item config Dflowinfo infuse_pressure_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo infuse_pressure_info_button*
    }
dui add dbutton $page_info 795 1050 \
    -bwidth 600 -bheight 350 -tags infuse_stop_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_infuse_stop -state normal
        dui item config Dflowinfo infuse_stop_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo infuse_stop_info_button*
    }

dui add dbutton $page_info 1425 1050 \
    -bwidth 230 -bheight 350 -tags pour_temp_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_pour_temp -state normal
        dui item config Dflowinfo pour_temp_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo pour_temp_info_button*
    }
dui add dbutton $page_info 1650 1050 \
    -bwidth 410 -bheight 350 -tags pour_limits_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_pour_limits -state normal
        dui item config Dflowinfo pour_limits_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo pour_limits_info_button*
    }
dui add dbutton $page_info 2085 1050 \
    -bwidth 600 -bheight 350 -tags pour_stop_info_button \
    -command {
        ::plugins::D_Flow_Espresso_Profile::reset_button_canvas
        dui item config Dflowinfo info_intro -state hidden
        dui item config Dflowinfo info_pour_stop -state normal
        dui item config Dflowinfo pour_stop_bg -outline $::plugins::D_Flow_Espresso_Profile::info_colour
        dui item hide Dflowinfo pour_stop_info_button*
    }

############### Adapted navigation buttons from original settings
### tabs
add_de1_text $page_name 380 100 -text [translate "PRESETS"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_variable $page_name 1010 80 -text "" -font $settings_tab_font -fill "#2d3046"  -justify "center" -anchor "center" -textvariable {[setting_profile_type_to_text]}
add_de1_variable $page_name 1010 130 -text "" -font Helv_7 -fill "#2d3046"  -justify "center" -anchor "center" -textvariable {[wrapped_profile_title]}
add_de1_text $page_name 1650 100 -text [translate "MACHINE"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_text $page_name 2270 100 -text [translate "APP"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_button $page_set {after 500 update_de1_explanation_chart; say [translate {settings}] $::settings(sound_button_in); set_next_page off "settings_1"; page_show off; set ::settings(active_settings_tab) "settings_1"; set_profiles_scrollbar_dimensions} 0 0 641 188
#add_de1_button $page_name {say [translate {save}] $::settings(sound_button_in); set ::settings(original_profile_title) $::settings(profile_title); if {$::settings(profile_has_changed) == 1} { borg toast [translate "Saved"]; save_profile; ::plugins::D_Flow_Espresso_Profile::demo_graph} } 642 0 1277 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_3; page_show settings_3; scheduler_feature_hide_show_refresh; set ::settings(active_settings_tab) "settings_3"} 1278 0 1904 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_4; page_show settings_4; set ::settings(active_settings_tab) "settings_4"; set_ble_scrollbar_dimensions; set_ble_scale_scrollbar_dimensions} 1905 0 2560 188

#set ::dflow_name_test [string range [ifexists ::settings(profile_title)] 9 end]
#add_de1_variable $page_name 1010 160 -text "" -font Helv_7 -fill "#2d3046"  -justify "center" -anchor "center" -textvariable {$::dflow_name_test}

dui add dbutton $page_name 642 0 \
    -bwidth 635 -bheight 188 \
    -command {
        if {$::settings(profile_has_changed) == 1} {
            set dflow_name_test [string range [ifexists ::settings(profile_title)] 9 end]
            if {$dflow_name_test in $::protected_d_flow_profiles == 1 } {
                set ::Dflow_message "$::settings(profile_title)\r[translate "is a protected profiles"]\r[translate "save as new file instead"]"
                after 3000 {set ::Dflow_message ""}
            } else {
                set ::settings(original_profile_title) $::settings(profile_title)
                borg toast [translate "Saved"]
                save_profile
                ::plugins::D_Flow_Espresso_Profile::demo_graph
            }
        }
    }

########## setting_2 tab button
dui add dbutton "settings_1 settings_3 settings_4" 642 0 1277 188 \
            -bwidth 635 -bheight 188 \
            -labelvariable {} -label_font [dui font get $font 12] -label_fill $font_colour -label_pos {0.5 0.5} \
            -command {
            set title_test [string range [ifexists ::settings(profile_title)] 0 7]
            if {$title_test == "A-Flow /" && [info commands ::plugins::A_Flow::prep] ne ""} {
                ::plugins::A_Flow::prep
                ::plugins::A_Flow::demo_graph
                if {$::settings(skin) == "DSx"} {
                    set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
                }
                dui page load Aflowset
            } elseif {$title_test == "D-Flow /" } {
                ::plugins::D_Flow_Espresso_Profile::prep
                ::plugins::D_Flow_Espresso_Profile::demo_graph
                if {$::settings(skin) == "DSx"} {
                    set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
                }
                dui page load Dflowset
            } else {
                after 500 update_de1_explanation_chart
                say [translate {settings}] $::settings(sound_button_in)
                set_next_page off $::settings(settings_profile_type)
                page_show off
                set ::settings(active_settings_tab) $::settings(settings_profile_type)
                fill_advanced_profile_steps_listbox
                set_advsteps_scrollbar_dimensions
                }
            }

### ok/cancel buttons
add_de1_text $page_set 2275 1520 -text [translate "Ok"] -font $botton_button_font -fill "#FFFFFF" -anchor "center"
add_de1_text $page_set 1760 1520 -text [translate "Cancel"] -font $botton_button_font -fill "#FFFFFF" -anchor "center"
add_de1_button $page_set {save_settings_to_de1; set_alarms_for_de1_wake_sleep; say [translate {save}] $::settings(sound_button_in); save_settings; profile_has_changed_set_colors;
    if {$::settings(skin) == "DSx"} {
        DSx_add_to_profile_settings_ok_button_enter
    }
    if {[ifexists ::profiles_hide_mode] == 1} {
        unset -nocomplain ::profiles_hide_mode
        fill_profiles_listbox
    }
    if {[ifexists ::settings_backup(calibration_flow_multiplier)] != [ifexists ::settings(calibration_flow_multiplier)]} {
        set_calibration_flow_multiplier $::settings(calibration_flow_multiplier)
    }
    if {[ifexists ::settings_backup(fan_threshold)] != [ifexists ::settings(fan_threshold)]} {
        set_fan_temperature_threshold $::settings(fan_threshold)
    }
    if {[ifexists ::settings_backup(water_refill_point)] != [ifexists ::settings(water_refill_point)]} {
        de1_send_waterlevel_settings
    }
    if {[array_item_difference ::settings ::settings_backup "steam_temperature steam_flow"] == 1} {
        # resend the calibration settings if they were changed
        de1_send_steam_hotwater_settings
        de1_enable_water_level_notifications
    }
    if {[array_item_difference ::settings ::settings_backup "enable_fahrenheit orientation screen_size_width saver_brightness use_finger_down_for_tap log_enabled hot_water_idle_temp espresso_warmup_timeout scale_bluetooth_address language skin waterlevel_indicator_on default_font_calibration waterlevel_indicator_blink display_rate_espresso display_espresso_water_delta_number display_group_head_delta_number display_pressure_delta_line display_flow_delta_line display_weight_delta_line allow_unheated_water display_time_in_screen_saver enabled_plugins plugin_tabs"] == 1  || [ifexists ::app_has_updated] == 1} {
        # changes that effect the skin require an app restart
        .can itemconfigure $::message_label -text [translate "Please quit and restart this app to apply your changes."]
        .can itemconfigure $::message_button_label -text [translate "Wait"]

        set_next_page off message; page_show message
        after 200 app_exit
    } else {
        if {[ifexists ::settings(settings_profile_type)] == "Aflowset"} {
            # if they were on the LIMITS tab of the Advanced profiles, reset the ui back to the main tab
            set ::settings(settings_profile_type) "Aflowset"
        }
        if {[ifexists ::settings(settings_profile_type)] == "Dflowset"} {
            # if they were on the LIMITS tab of the Advanced profiles, reset the ui back to the main tab
            set ::settings(settings_profile_type) "Dflowset"
        }

        #set_next_page off off; page_show off
        if {$::settings(skin) == "DSx"} {
            DSx_add_to_profile_settings_ok_button_leave
        } else {
            set_next_page off off; page_show off
        }
    }
} 2016 1430 2560 1600
add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); set_next_page off off; page_show off; fill_advanced_profile_steps_listbox;restore_espresso_chart; save_settings_to_de1; fill_profiles_listbox ; fill_extensions_listbox} 1505 1430 2015 1600



########## settings_1 page
dui add dbutton settings_1 1100 526 \
    -bwidth 200 -bheight 200 -tags new_profile_button -initial_state hidden \
    -command {
        set title_test [string range [ifexists ::settings(profile_title)] 0 7]
        if {$title_test == "A-Flow /" && [info commands ::plugins::A_Flow::prep] ne ""} {
            ::plugins::A_Flow::prep
            ::plugins::A_Flow::demo_graph
            if {$::settings(skin) == "DSx"} {
                set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
            }
            dui page load Aflowset
        } elseif {$title_test == "D-Flow /" } {
            ::plugins::D_Flow_Espresso_Profile::prep
            ::plugins::D_Flow_Espresso_Profile::demo_graph
            if {$::settings(skin) == "DSx"} {
                set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
            }
            dui page load Dflowset
        }
    }

dui add dbutton "settings_1" 1330 220 \
    -bwidth 1230 -bheight 580 \
    -labelvariable {} -label_font [dui font get $font 12] -label_fill $font_colour -label_pos {0.5 0.5} \
    -command {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" && [info commands ::plugins::A_Flow::prep] ne ""} {
        ::plugins::A_Flow::prep
        ::plugins::A_Flow::demo_graph
        if {$::settings(skin) == "DSx"} {
            set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
        }
        dui page load Aflowset
    } elseif {$title_test == "D-Flow /" } {
                ::plugins::D_Flow_Espresso_Profile::prep
                ::plugins::D_Flow_Espresso_Profile::demo_graph
                if {$::settings(skin) == "DSx"} {
                    set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
                }
                dui page load Dflowset
    } else {
        after 500 update_de1_explanation_chart
        say [translate {settings}] $::settings(sound_button_in)
        set_next_page off $::settings(settings_profile_type)
        page_show off
        set ::settings(active_settings_tab) $::settings(settings_profile_type)
        fill_advanced_profile_steps_listbox
        set_advsteps_scrollbar_dimensions
        }
    }

add_de1_widget "settings_1c" graph 1330 300 {
    set ::preview_graph_advanced $widget
    update_de1_explanation_chart;
    $widget element create line_espresso_de1_explanation_chart_pressure -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_espresso_de1_explanation_chart_flow -xdata espresso_de1_explanation_chart_elapsed_flow -ydata espresso_de1_explanation_chart_flow  -label "" -linewidth [rescale_x_skin 12] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_espresso_de1_explanation_chart_temp -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_temperature_10  -label "" -linewidth [rescale_x_skin 10] -color #ff888c  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;

    $::preview_graph_advanced axis configure x -color #5a5d75 -tickfont Helv_6;
    $::preview_graph_advanced axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "Advanced"] -titlefont Helv_8 -titlecolor #5a5d75;
    bind $::preview_graph_advanced [platform_button_press] {
        set title_test [string range [ifexists ::settings(profile_title)] 0 7]
        if {$title_test == "A-Flow /" && [info commands ::plugins::A_Flow::prep] ne ""} {
            ::plugins::A_Flow::prep
            ::plugins::A_Flow::demo_graph
            if {$::settings(skin) == "DSx"} {
                set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
            }
            dui page load Aflowset
        } elseif {$title_test == "D-Flow /" } {
                ::plugins::D_Flow_Espresso_Profile::prep
                ::plugins::D_Flow_Espresso_Profile::demo_graph
                if {$::settings(skin) == "DSx"} {
                    set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
                }
                dui page load Dflowset
        } else {
            after 500 update_de1_explanation_chart
            say [translate {settings}] $::settings(sound_button_in)
            set_next_page off $::settings(settings_profile_type)
            page_show off
            set ::settings(active_settings_tab) $::settings(settings_profile_type)
            fill_advanced_profile_steps_listbox
            set_advsteps_scrollbar_dimensions
            }
        }
} -plotbackground #fff -width [rescale_x_skin 1050] -height [rescale_y_skin 450] -borderwidth 1 -background #FFFFFF -plotrelief raised  -plotpady 0 -plotpadx 10

add_de1_button "settings_1" {say [translate {temperature}] $::settings(sound_button_in); change_espresso_temperature 0.5; profile_has_changed_set } 2380 230 2590 480
add_de1_button "settings_1" {say [translate {temperature}] $::settings(sound_button_in); change_espresso_temperature -0.5; profile_has_changed_set } 2380 490 2590 800


##### Profile Conversion code

dui add dbutton Dflowset 0 1450 \
    -bwidth 200 -bheight 150 \
    -command {
        if {[.can itemcget dflow_convert -state] eq "hidden"} {
            dui item config Dflowset dflow_convert* -initial_state normal -state normal
        } else {
            dui item config Dflowset dflow_convert* -initial_state hidden -state hidden
        }
    }

dui add dbutton Dflowset 760 570 \
    -bwidth 210 -bheight 160 -tags dflow_convert -initial_state hidden \
    -shape outline -width 2 -outline #eee \
    -label "convert to\rAdvanced\rprofile" -label_font [dui font get Roboto-Regular 14] -label_fill #7f879a -label_pos {0.5 0.5} \
    -command {
        ::plugins::D_Flow_Espresso_Profile::save_D-Flow_to_Advanced
    }

proc save_D-Flow_to_Advanced {} {
    if {$::DFlow_name == ""} {
        set ::Dflow_message "we need a profile name"
        after 1200 {set ::Dflow_message ""}
        return
    }
    set profile_filename $::DFlow_name
    if {[file exists "[data_directory]/profiles/${profile_filename}.tcl"] != 1} {
        set ::settings(profile_title) $::DFlow_name;
        borg toast [translate "D-Flow converted to Advance"]
        save_profile
        dui page load settings_2c
    } else {
        set ::Dflow_message "$::DFlow_name [translate "already exists"]"
        after 1200 {set ::Dflow_message ""}
    }
}


######## Q
blt::vector create Q_demo_elapsed Q_demo_pressure Q_demo_flow Q_demo_flow_weight

set ::Q_demo_elapsed {0.028 0.27 0.508 0.746 1.019 1.256 1.498 1.767 2.007 2.25 2.517 2.76 2.997 3.268 3.507 3.754 4.02 4.262 4.501 4.771 5.01 5.248 5.519 5.758 5.998 6.268 6.508 6.752 7.019 7.26 7.5 7.769 8.01 8.25 8.518 8.766 8.997 9.269 9.509 9.748 10.02 10.263 10.497 10.768 11.007 11.249 11.519 11.768 12.002 12.27 12.511 12.749 13.021 13.257 13.501 13.774 14.007 14.278 14.518 14.757 14.999 15.268 15.507 15.748 16.025 16.259 16.5 16.767 17.016 17.248 17.52 17.763 17.997 18.285 18.525 18.747 19.017 19.27 19.503 19.771 20.009 20.256 20.527 20.757 21.001 21.267 21.536}
set ::Q_demo_pressure {0.0 0.69 0.77 0.79 0.85 0.83 0.85 0.9 0.86 0.89 0.94 0.98 1.03 1.09 1.14 1.19 1.27 1.36 1.46 1.59 1.76 1.96 2.27 2.68 3.14 3.66 4.27 5.01 5.7 6.27 6.76 7.13 7.41 7.57 7.65 7.67 7.66 7.6 7.56 7.47 7.38 7.26 7.1 6.96 6.82 6.68 6.49 6.34 6.19 6.03 5.87 5.76 5.61 5.46 5.37 5.24 5.12 5.02 4.94 4.86 4.77 4.71 4.63 4.58 4.52 4.45 4.4 4.35 4.31 4.25 4.21 4.18 4.15 4.22 4.07 4.11 4.09 4.07 4.03 4.0 4.0 3.98 4.01 3.97 3.92 3.89 3.92 3.92}
set ::Q_demo_flow {0.0 6.84 7.11 7.41 7.55 7.66 7.89 7.94 7.96 8.04 8.04 8.04 8.04 8.04 8.04 8.04 8.04 7.98 7.97 7.95 7.88 7.86 7.79 7.68 7.58 7.46 7.29 7.12 6.86 6.4 5.75 5.12 4.5 3.98 3.52 3.15 2.84 2.59 2.4 2.26 2.16 2.05 1.98 1.94 1.89 1.85 1.83 1.83 1.8 1.79 1.78 1.77 1.77 1.77 1.78 1.76 1.77 1.77 1.77 1.76 1.78 1.76 1.79 1.79 1.78 1.77 1.78 1.79 1.79 1.79 1.79 1.79 1.87 1.79 1.79 1.79 1.82 1.82 1.82 1.84 1.81 1.8 1.8 1.8 1.82 1.84 1.8 1.77}
set ::Q_demo_flow_weight {0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.18 0.43 0.61 0.81 0.94 1.09 1.32 1.45 1.6 1.7 1.83 1.93 2.0 2.16 2.23 2.31 2.31 2.26 2.24 2.23 2.22 2.20 2.19 2.18 2.17 2.16 2.15 2.14 2.13 2.12 2.11 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.1 2.03 2.1 2.1 2.1 2.1}


add_de1_widget "settings_1" graph 1330 300 {
    set ::Q_demo_graph $widget
    $widget element create line_Q_demo_pressure -xdata $::Q_demo_elapsed -ydata $::Q_demo_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_Q_demo_flow -xdata $::Q_demo_elapsed -ydata $::Q_demo_flow  -label "" -linewidth [rescale_x_skin 10] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_Q_demo_flow_weight -xdata $::Q_demo_elapsed -ydata $::Q_demo_flow_weight  -label "" -linewidth [rescale_x_skin 10] -color #A1663A  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;

    $::Q_demo_graph axis configure x -color #5a5d75 -tickfont Helv_6;
    $::Q_demo_graph axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "D-Flow"] -titlefont Helv_8 -titlecolor #5a5d75;
    bind $::Q_demo_graph [platform_button_press] {}
} -plotbackground #fff -width [rescale_x_skin 1050] -height [rescale_y_skin 420] -borderwidth 1 -background #FFFFFF -plotrelief raised  -plotpady 0 -plotpadx 10 -initial_state hidden -tags Q_demo_graph

proc hide_Q_demo_graph {} {
    .can itemconfigure Q_demo_graph -state hidden
    dui item config off Q_demo_graph -initial_state hidden
}
proc show_Q_demo_graph {} {
    .can itemconfigure Q_demo_graph -state normal
    dui item config off Q_demo_graph -initial_state normal
}

::plugins::D_Flow_Espresso_Profile::hide_Q_demo_graph

######## La_Pavoni
blt::vector create La_Pavoni_demo_elapsed La_Pavoni_demo_pressure La_Pavoni_demo_flow La_Pavoni_demo_flow_weight

set ::La_Pavoni_demo_elapsed {0.027 0.265 0.504 0.776 1.027 1.254 1.53 1.767 2.007 2.277 2.517 2.756 3.028 3.268 3.505 3.783 4.016 4.254 4.527 4.774 5.006 5.28 5.517 5.755 6.031 6.268 6.507 6.787 7.021 7.257 7.526 7.769 8.037 8.277 8.515 8.784 9.025 9.267 9.535 9.785 10.047 10.287 10.532 10.768 11.046 11.28 11.518 11.788 12.028 12.267 12.537 12.779 13.02 13.285 13.524 13.767 14.036 14.276 14.515 14.787 15.035 15.265 15.54 15.78 16.018 16.287 16.529 16.767 17.037 17.278 17.517 17.789 18.027 18.268 18.537 18.778 19.023 19.286 19.527 19.773 20.035 20.28 20.516 20.785 21.036 21.267 21.536 21.779 22.022 22.286 22.526 22.771 23.038 23.275 23.519 23.79 24.029 24.274 24.537 24.781 25.021 25.287 25.532 25.767 26.04 26.284 26.518 26.789 27.028 27.267 27.535 27.777 28.017 28.285 28.526 28.771 29.037 29.275 29.52 29.785 30.028 30.302 30.537 30.78 31.056 31.287 31.529 31.797 32.037 32.283 32.547 32.789 33.028 33.316 33.559 33.778 34.049 34.286 34.532 34.801 35.038 35.281 35.548 35.792 36.028 36.299 36.535 36.57 36.805 37.078 37.316 37.557 37.827 38.066 38.304 38.586 38.815 39.056 39.332 39.593 39.805 40.078 40.315 40.588 40.831 41.067 41.306 41.576 41.824 42.056 42.329 42.567 42.807 43.078 43.319 43.563 43.831 44.067 44.306 44.584 44.816 45.055 45.329 45.567 45.811 46.076 46.331 46.562 46.827 47.067 47.306 47.578 47.817 48.056 48.332 48.565 48.836 49.077 49.317 49.585 49.826 50.067 50.342 50.576 50.818 51.086 51.331 51.567 51.838 52.076 52.317 52.587 52.828 53.069 53.342 53.577 53.816 54.088 54.337 54.572 54.837 55.085 55.319 55.59 55.826 56.066 56.336 56.581 56.837 57.085 57.328 57.566 57.837 58.075 58.317 58.591 58.844 59.068 59.336}
set ::La_Pavoni_demo_pressure {0.0 0.05 0.02 0.0 0.03 0.07 0.14 0.34 0.39 0.43 0.51 0.59 0.65 0.75 0.81 0.92 1.03 1.09 1.13 1.18 1.21 1.17 1.15 1.12 1.1 1.06 1.0 0.97 1.01 1.01 0.96 0.92 0.92 0.92 0.9 0.9 0.9 0.88 0.87 0.87 0.87 0.9 0.93 0.91 0.9 0.9 0.89 0.88 0.88 0.87 0.87 0.86 0.91 0.93 0.91 0.9 0.9 0.89 0.88 0.87 0.88 0.88 0.87 0.88 0.9 0.92 0.93 0.94 0.96 0.97 0.99 1.04 1.06 1.11 1.15 1.16 1.18 1.21 1.26 1.3 1.35 1.34 1.35 1.36 1.35 1.34 1.34 1.33 1.33 1.32 1.32 1.31 1.31 1.32 1.32 1.31 1.31 1.31 1.3 1.3 1.3 1.29 1.28 1.26 1.25 1.26 1.26 1.24 1.24 1.22 1.22 1.21 1.2 1.2 1.21 1.21 1.2 1.19 1.19 1.18 1.18 1.18 1.17 1.16 1.15 1.14 1.14 1.14 1.14 1.13 1.13 1.13 1.13 1.12 1.12 1.11 1.1 1.1 1.1 1.09 1.08 1.09 1.08 1.07 1.07 1.07 1.06 1.05 1.05 1.06 1.08 1.1 1.12 1.18 1.19 1.18 1.17 1.17 1.18 1.24 1.4 1.63 1.86 2.04 2.21 2.4 2.64 2.88 3.09 3.31 3.54 3.78 4.05 4.37 4.69 5.02 5.35 5.66 6.0 6.27 6.53 6.79 6.94 7.11 7.21 7.22 7.25 7.25 7.16 7.06 6.96 6.77 6.66 6.56 6.43 6.24 6.18 6.11 6.02 5.88 5.8 5.73 5.67 5.57 5.45 5.44 5.39 5.33 5.27 5.21 5.14 5.09 5.04 5.0 4.95 4.89 4.86 4.83 4.77 4.74 4.69 4.7 4.68 4.69 4.66 4.67 4.64 4.61 4.59 4.6 4.54 4.52 4.53 4.47 4.48 4.47 4.46 4.42 4.38 4.37}
set ::La_Pavoni_demo_flow {0.0 0.57 1.51 2.83 3.96 4.75 5.39 5.95 6.3 6.57 6.84 7.0 7.26 7.44 7.62 7.61 7.41 7.0 6.45 5.78 5.03 4.32 3.66 3.05 2.53 1.99 1.55 1.27 1.08 0.84 0.66 0.51 0.4 0.31 0.24 0.19 0.15 0.11 0.09 0.07 0.05 0.18 0.16 0.12 0.09 0.07 0.06 0.04 0.03 0.03 0.02 0.05 0.15 0.13 0.1 0.08 0.06 0.05 0.04 0.03 0.02 0.02 0.01 0.01 0.01 0.01 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.05 0.27 0.54 0.83 1.11 1.34 1.55 1.69 1.83 1.95 2.02 2.11 2.2 2.24 2.29 2.37 2.4 2.36 2.39 2.42 2.46 2.46 2.47 2.43 2.43 2.47 2.48 2.49 2.51 2.52 2.52 2.57 2.52 2.55 2.56 2.52 2.49 2.52 2.49 2.47 2.47 2.44 2.39 2.37 2.39 2.38 2.34 2.3 2.33 2.34 2.37 2.31 2.34 2.37 2.34 2.36 2.3 2.33 2.34 2.37 2.35 2.37 2.36 2.35 2.37 2.36 2.38 2.39 2.33 2.33 2.32 2.37 2.31 2.31 2.33 2.34 2.33 2.36 2.33 2.39 2.37 2.37 2.36 2.33 2.35 2.31 2.35 2.36 2.36 2.38 2.4}
set ::La_Pavoni_demo_flow_weight {0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 -0.03 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.03 0.03 0.03 0.03 0.03 0.03 0.03 0.05 0.05 0.08 0.08 0.1 0.1 0.1 0.1 0.1 0.1 0.08 0.08 0.1 0.1 0.1 0.1 0.08 0.1 0.08 0.1 0.1 0.1 0.13 0.13 0.13 0.15 0.15 0.2 0.2 0.3 0.33 0.45 0.48 0.58 0.71 0.76 0.86 1.01 1.14 1.26 1.39 1.47 1.67 1.77 1.9 1.92 2.12 2.12 2.25 2.3 2.38 2.48 2.5 2.53 2.58 2.63 2.63 2.65 2.65 2.68 2.65 2.68 2.68 2.65 2.65 2.65 2.68 2.65 2.65 2.63 2.63 2.6 2.6 2.6 2.58 2.6 2.58 2.58 2.55 2.58 2.58 2.55 2.55 2.55 2.53 2.53 2.55 2.55 2.58 2.55 2.55 2.53 2.53 2.53 2.53 2.5 2.53 2.53}


add_de1_widget "settings_1" graph 1330 300 {
    set ::La_Pavoni_demo_graph $widget
    $widget element create line_La_Pavoni_demo_pressure -xdata $::La_Pavoni_demo_elapsed -ydata $::La_Pavoni_demo_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_La_Pavoni_demo_flow -xdata $::La_Pavoni_demo_elapsed -ydata $::La_Pavoni_demo_flow  -label "" -linewidth [rescale_x_skin 10] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
    $widget element create line_La_Pavoni_demo_flow_weight -xdata $::La_Pavoni_demo_elapsed -ydata $::La_Pavoni_demo_flow_weight  -label "" -linewidth [rescale_x_skin 10] -color #A1663A  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;

    $::La_Pavoni_demo_graph axis configure x -color #5a5d75 -tickfont Helv_6;
    $::La_Pavoni_demo_graph axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "D-Flow"] -titlefont Helv_8 -titlecolor #5a5d75;
    bind $::La_Pavoni_demo_graph [platform_button_press] {}
} -plotbackground #fff -width [rescale_x_skin 1050] -height [rescale_y_skin 420] -borderwidth 1 -background #FFFFFF -plotrelief raised  -plotpady 0 -plotpadx 10 -initial_state hidden -tags La_Pavoni_demo_graph

proc hide_La_Pavoni_demo_graph {} {
    .can itemconfigure La_Pavoni_demo_graph -state hidden
    dui item config off La_Pavoni_demo_graph -initial_state hidden
}
proc show_La_Pavoni_demo_graph {} {
    .can itemconfigure La_Pavoni_demo_graph -state normal
    dui item config off La_Pavoni_demo_graph -initial_state normal
}

::plugins::D_Flow_Espresso_Profile::hide_La_Pavoni_demo_graph

################ Mod external code
if {$::settings(skin) == "DSx"} {
    if {[file exists "[skin_directory]/DSx_Home_Page/DSx_2021_home.page"] == 1 && $::DSx_settings(DSx_home) == "2021home"} {
        dui add variable off [expr {$::DSx_data_x + 480}] [expr {$::DSx_data_y + 672}] -anchor "center" -justify "center" -font [DSx_font font 7] -fill $::DSx_settings(font_colour) -textvariable {[::plugins::D_Flow_Espresso_Profile::D-Flow_data]}
    } elseif {$::DSx_home_page_version == {} } {
        #dui add variable off 400 682 -anchor "center" -justify "center" -font [DSx_font font 7] -fill $::DSx_settings(font_colour) -textvariable {[::plugins::D_Flow_Espresso_Profile::D-Flow_data]}
    }

    trace add execution load_bluecup {leave} ::plugins::D_Flow_Espresso_Profile::prep
    trace add execution load_pinkcup {leave} ::plugins::D_Flow_Espresso_Profile::prep
    trace add execution load_orangecup {leave} ::plugins::D_Flow_Espresso_Profile::prep
}

set ::Q_profile_notes ""
set ::La_Pavoni_profile_notes ""
add_de1_variable "settings_1" 1360 890 -text "" -font Helv_6 -fill "#7f879a" -justify "left" -anchor "nw"  -width [rescale_y_skin 1150] -textvariable {$::Q_profile_notes}
add_de1_variable "settings_1" 1360 900 -text "" -font Helv_6 -fill "#7f879a" -justify "left" -anchor "nw"  -width [rescale_y_skin 1150] -textvariable {$::La_Pavoni_profile_notes}
dui add dbutton "settings_1" 1350 820 2530 1180 -tags Q_notes_button -initial_state hidden -command {web_browser "https://coffee.brakel.com.au/"}
dui add dbutton "settings_1" 1350 820 2530 1180 -tags La_Pavoni_notes_button -initial_state hidden -command {web_browser "https://coffee.brakel.com.au/"}

rename ::update_de1_explanation_chart ::update_de1_explanation_chart_default
proc ::update_de1_explanation_chart {} {
    if {$::settings(settings_profile_type) == "settings_2a" || $::settings(settings_profile_type) == "settings_2b"} {
	    ::plugins::D_Flow_Espresso_Profile::hide_Q_demo_graph
        ::plugins::D_Flow_Espresso_Profile::hide_La_Pavoni_demo_graph
        set ::Q_profile_notes ""
        set ::La_Pavoni_profile_notes ""
        dui item config settings_1 Q_notes_button* -initial_state hidden -state hidden
        dui item config settings_1 La_Pavoni_notes_button* -initial_state hidden -state hidden
	}
    ::update_de1_explanation_chart_default
}
rename ::update_de1_plus_advanced_explanation_chart ::update_de1_plus_advanced_explanation_chart_default
proc ::update_de1_plus_advanced_explanation_chart {} {

	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" } {
        ::plugins::D_Flow_Espresso_Profile::demo_graph
    } else {
        ::update_de1_plus_advanced_explanation_chart_default
    }
    if {$::settings(profile_title) == "D-Flow / Q" && [dui page current] == "settings_1"} {
        ::plugins::D_Flow_Espresso_Profile::hide_La_Pavoni_demo_graph
        ::plugins::D_Flow_Espresso_Profile::show_Q_demo_graph
        set ::La_Pavoni_profile_notes ""
        set ::Q_profile_notes {This is the profile Damian currently uses for every day coffee
Grind 10um causer than other profiles, more berry flavours from "Xtraction Blend" than Slayer V3 or La Marzocco KB90, same creaminess.
The Preview graph above shows what a typical shot graph should look like

The goal is to grind for a pressure peak between 6 and 9 bar
     tap to visit "https://coffee.brakel.com.au/"
        }
        dui item config settings_1 Q_notes_button* -initial_state normal -state normal
    } elseif {$::settings(profile_title) == "D-Flow / La Pavoni" && [dui page current] == "settings_1"} {
        ::plugins::D_Flow_Espresso_Profile::hide_Q_demo_graph
        ::plugins::D_Flow_Espresso_Profile::show_La_Pavoni_demo_graph
        set ::Q_profile_notes ""
        set ::La_Pavoni_profile_notes {Damian created this profile to simulate the results from his La Pavoni machine
which he profiled side by side with a Slayer V3 for same taste in cup.
The Preview graph above shows what a typical shot graph should look like

The goal is to grind for a pressure peak between 6 and 9 bar
        }
        dui item config settings_1 La_Pavoni_notes_button* -initial_state normal -state normal
    } else {
        ::plugins::D_Flow_Espresso_Profile::hide_Q_demo_graph
        ::plugins::D_Flow_Espresso_Profile::hide_La_Pavoni_demo_graph
        set ::Q_profile_notes ""
        set ::La_Pavoni_profile_notes ""
        dui item config settings_1 Q_notes_button* -initial_state hidden -state hidden
        dui item config settings_1 La_Pavoni_notes_button* -initial_state hidden -state hidden
    }
}

rename ::setting_profile_type_to_text ::setting_profile_type_to_text_default
proc ::setting_profile_type_to_text {} {
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" || $title_test == "A-Flow /"} {
        dui item show settings_1 new_profile_button*
        $::globals(widget_profile_name_to_save) configure -state disabled
    } else {
        dui item hide settings_1 new_profile_button*
        $::globals(widget_profile_name_to_save) configure -state normal
    }
    if {$title_test == "D-Flow /" } {
        setting_profile_type_to_text_default
        .can coords $::tab1_profile_label [lindex [.can coords $::tab1_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab2_profile_label [lindex [.can coords $::tab2_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab3_profile_label [lindex [.can coords $::tab3_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab4_profile_label [lindex [.can coords $::tab4_profile_label] 0] [rescale_y_skin 80]
        $::preview_graph_advanced axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "D-Flow"] -titlefont Helv_8 -titlecolor #5a5d75;
        return "D-FLOW"
    } else {
        $::preview_graph_advanced axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "Advanced"] -titlefont Helv_8 -titlecolor #5a5d75;
        setting_profile_type_to_text_default
    }
}

rename ::wrapped_profile_title ::wrapped_profile_title_default
proc ::wrapped_profile_title {} {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "D-Flow /" } {
        set dft [string range [ifexists ::settings(profile_title)] 9 35]
        return $dft
    } else {
        wrapped_profile_title_default
    }
}

proc show_d {args} {
    if {[dui page current] == "settings_2c"} {
        set title_test [string range [ifexists ::settings(profile_title)] 0 7]
        if {$title_test == "D-Flow /" } {
            ::plugins::D_Flow_Espresso_Profile::prep
            ::plugins::D_Flow_Espresso_Profile::demo_graph
            if {$::settings(skin) == "DSx"} {
                set ::settings(grinder_dose_weight) [round_to_one_digits $::DSx_settings(bean_weight)]
            }
            dui page load Dflowset
        }
    }
}
trace add execution show_settings {leave} ::plugins::D_Flow_Espresso_Profile::show_d

dui add dbutton settings_1 2300 1220 \
    -bwidth 250 -bheight 190 \
    -command {
        set title_test [string range [ifexists ::settings(profile_title)] 0 7]
        if {$title_test != "D-Flow /" } {
            popup [translate "Saved"]
            save_profile
        } else {
            popup [translate "disabled for D-Flow profiles"]
        }
    }

}



