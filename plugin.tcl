### By Damian Brakel ###
set plugin_name "A_Flow"

namespace eval ::plugins::${plugin_name} {
    # These are shown in the plugin selection page
    variable author "Janek"
    variable contact "via Diaspora"
    variable description "A-Flow is a simple to use advanced profile based on D-Flow and 'adaptive for medium roasts' profile"
    variable version 1.3
    variable min_de1app_version {1.45.0.47}


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
the group with water at boiler pressure. to emulater this you would set a 1 bar infuse pressure.

Some machines use pressures anywhere in between.

Other machines may not hold an infusion presure at all, like a common E61 pump machines
where it apply water straight to extraction pressure, typically 8 or 9 bar.
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
4g to 6g is a good starting range to experiment with.
}

set info_pour_temp {
Higher temperature helps extract the soluble compounds, however it also increases
harsher tasts that may exist too.

Higher temperatures usually results in lower body with more pronounced acidity, wine, fruity, vegetal flavours.
Lower temperatures often work better with slower extraction rates.
}

set info_pour_limits {
A-Flow uses a pressure ramp to slowly increase the extraction flow after infusion phase. 
If enabled followed by a pressure decline step to reach a defined extraction flow rate. 
Final extraction flow is either slowly increase or decreasing.

It's possible to skip the pressure increase and decline by setting the pouring time to 0. 
This lead to a similar profile as D-Flow. 

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
set page_name {Aflowset Aflowinfo}
set page_set {Aflowset}
set page_info {Aflowinfo}
set font "Roboto-Regular"
set font_colour #7f879a
set icon_colour #7f879a
set info_colour #ff9421
set button_outline_colour #eee
set button_outline_width 2
set ::Aflow_message ""
if {[info exists ::settings(A_Flow_graph_style)] == 0} {
    set ::settings(A_Flow_graph_style) "DSx"
}
if {$::settings(skin) == "DSx"} {
    set ::DSx_settings(orange_cup_indicator) { }
    set ::DSx_settings(blue_cup_indicator) { }
    set ::DSx_settings(pink_cup_indicator) { }
    clear_profile_font
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); back_to_previous_page; fill_advanced_profile_steps_listbox; restore_espresso_chart; LRv2_preview; DSx_graph_restore; save_settings_to_de1; fill_profiles_listbox; fill_extensions_listbox; refresh_DSx_temperature; ::plugins::A_Flow::prep} 1505 1430 2015 1600
    } else {
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); set_next_page off off; page_show off; fill_advanced_profile_steps_listbox; restore_espresso_chart; save_settings_to_de1; fill_profiles_listbox ; fill_extensions_listbox; ::plugins::A_Flow::prep} 1505 1430 2015 1600
}


################ procedures
proc check_profiles_exist {} {
    # check for all files in profiles folder if they exist in global profile folder and copy them if not
    set plugins_profiles_folder [homedir]/plugins/A_Flow/profiles/
    set global_profiles_folder [homedir]/profiles/
    set default_profiles_files [glob -nocomplain $plugins_profiles_folder*.tcl]
    foreach profile_file $default_profiles_files {
        set profile_name [file tail $profile_file]
        if {[file exists $global_profiles_folder$profile_name] != 1} {
            file copy -force $profile_file $global_profiles_folder
        }
    }
}

proc main {} {
    check_profiles_exist
}

proc check_Roboto-Regular_exists {} {
    if {[file exists "[homedir]/fonts/Roboto-Regular.ttf"] != 1} {
        file copy -force [homedir]/skins/DSx/DSx_Font_Files/Roboto-Regular.ttf [homedir]/fonts/Roboto-Regular.ttf
    }
}
check_Roboto-Regular_exists




### Check / write profile
proc prep { args } {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        array set filling [lindex $::settings(advanced_shot) 0]
        array set soaking [lindex $::settings(advanced_shot) 1]
        array set ramp_up [lindex $::settings(advanced_shot) 2]
        array set ramp_down [lindex $::settings(advanced_shot) 3]
        array set pouring_start [lindex $::settings(advanced_shot) 4]
        set ::Aflow_filling_temperature $filling(temperature)
        set ::Aflow_filling_flow $filling(flow)
        set ::Aflow_soaking_seconds [round_to_one_digits $soaking(seconds)]        
        set ::Aflow_soaking_pressure $soaking(pressure)
        set ::Aflow_soaking_volume $soaking(volume)
        set ::Aflow_soaking_weight $soaking(weight)
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$ramp_up(seconds) + $ramp_down(seconds)}]]
        set ::Aflow_pouring_flow [round_to_one_digits $pouring_start(flow)]
        set ::Aflow_pouring_pressure $ramp_up(pressure)
        set ::Aflow_pouring_temperature $ramp_up(temperature)
        set ::Aflow_ramp_down_pressure $ramp_down(pressure)
        if {$ramp_down(seconds) > 0} {
            set ::ramp_down_enabled true
        } else {
            set ::ramp_down_enabled false
        }
        array set pouring [lindex $::settings(advanced_shot) 5]
        if {$pouring(flow) > $::Aflow_pouring_flow} {
            set ::flow_extraction_up true
        } else {
            set ::flow_extraction_up false
        }
    }
}

proc update_A-Flow {} {
    array set filling [lindex $::settings(advanced_shot) 0]
    array set soaking [lindex $::settings(advanced_shot) 1]
    array set ramp_up [lindex $::settings(advanced_shot) 2]
    array set ramp_down [lindex $::settings(advanced_shot) 3]
    array set pouring_start [lindex $::settings(advanced_shot) 4]
    array set pouring [lindex $::settings(advanced_shot) 5]
    set filling(temperature) $::Aflow_filling_temperature
    set soaking(temperature) $::Aflow_filling_temperature
    set soaking(pressure) $::Aflow_soaking_pressure
    set soaking(seconds) $::Aflow_soaking_seconds
    set soaking(volume) $::Aflow_soaking_volume
    set soaking(weight) $::Aflow_soaking_weight
    
    set ramp_up(temperature) $::Aflow_pouring_temperature
    set ramp_up(pressure) $::Aflow_pouring_pressure

    set ramp_down(temperature) $::Aflow_pouring_temperature
    set ramp_down(exit_flow_under) [round_to_one_digits [expr {$::Aflow_pouring_flow + 0.1}]]
    if {$::ramp_down_enabled} {
        set ramp_up(seconds) [round_to_integer [expr {$::Aflow_ramp_updown_seconds / 2}]]
        set ramp_down(seconds) [round_to_integer [expr {($::Aflow_ramp_updown_seconds / 2) + ($::Aflow_ramp_updown_seconds % 2 ? 1 : 0)}]]
        set ramp_up(exit_flow_over) [round_to_one_digits [expr {$::Aflow_pouring_flow * 2}]] 
    } else {
        set ramp_up(seconds) [round_to_integer [expr {$::Aflow_ramp_updown_seconds}]]
        set ramp_down(seconds) 0
        set ramp_up(exit_flow_over) [round_to_one_digits [expr {$::Aflow_pouring_flow}]]  
    }

    set pouring_start(temperature) $::Aflow_pouring_temperature
    set pouring_start(flow) $::Aflow_pouring_flow

    if {$ramp_up(seconds) < 1} {
        # activate pouring start step to allow fast transition to target flow
        set pouring_start(seconds) 10
        set pouring_start(exit_flow_over) [round_to_one_digits [expr {$::Aflow_pouring_flow - 0.1}]]
        set pouring_start(exit_type) flow_over
        set pouring_start(exit_if) 1
    } else {
        # disable step in case ramp up is used
        set pouring_start(seconds) 0
    }

    set pouring(temperature) $::Aflow_pouring_temperature
    if {$::flow_extraction_up} {
        set pouring(flow) [round_to_one_digits [expr {$::Aflow_pouring_flow * 2}]] 
    } else {
        set pouring(flow) 0
    }
    
    set pouring(max_flow_or_pressure) $::Aflow_pouring_pressure

    set newprofile {}
    lappend newprofile [array get filling]
    lappend newprofile [array get soaking]
    lappend newprofile [array get ramp_up]
    lappend newprofile [array get ramp_down]
    lappend newprofile [array get pouring_start]
    lappend newprofile [array get pouring]
    set ::settings(advanced_shot) $newprofile
    range_check_shot_variables
    profile_has_changed_set
    ::plugins::A_Flow::demo_graph
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

proc save_A-Flow_profile {} {
    set pre "A-Flow____"
    set df "A-Flow / "
    set profile_filename $pre$::AFlow_name
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {[file exists "[homedir]/profiles/${profile_filename}.tcl"] != 1} {
        if {$title_test == "A-Flow /" } {
            set ::settings(profile_title) $df$::AFlow_name;
        } else {
            set ::settings(profile_title) $::AFlow_name;
        }
        borg toast [translate "Saved"]
        save_profile
        ::plugins::A_Flow::demo_graph

    } else {
        set ::Aflow_message "$df$::AFlow_name [translate "already exists"]"
        after 1200 {set ::Aflow_message ""}
    }

}

proc tap_to_update {} {
    if {$::settings(profile_has_changed) == 1} {
        return "*tap here to save changes*"
    } else {
        return ""
    }
}

proc A-Flow_data {} {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        set a [round_to_integer $::Aflow_filling_temperature]
        set b [return_temperature_setting $::Aflow_pouring_temperature]
        set c [::plugins::A_Flow::format_SAW $::Aflow_soaking_weight]
        set d [return_flow_measurement $::Aflow_pouring_flow]
        set f [return_pressure_measurement $::Aflow_pouring_pressure]
        set s {  }
        set m {-}
        return $a$m$b$s$s$c$s$s$d$s$f
    }
}

proc toggle_graph {} {
    if {$::settings(A_Flow_graph_style) == "Insight"} {
        set ::settings(A_Flow_graph_style) "DSx"
    } else {
        set ::settings(A_Flow_graph_style) "Insight"
    }
    ::plugins::A_Flow::select_flow_curve
}
proc select_flow_curve {} {
    if {$::settings(A_Flow_graph_style) == "Insight"} {
        $::Aflow_demo_graph element configure line_espresso_de1_explanation_chart_flow -ydata espresso_de1_explanation_chart_flow
        $::Aflow_demo_graph axis configure x -color #5a5d75
        $::Aflow_demo_graph axis configure y -color #5a5d75
        $::Aflow_demo_graph axis configure y2 -hide 1
        $::Aflow_demo_graph grid configure -color #ddd
        $::Aflow_demo_graph configure -plotbackground #f8f8f8 -background #fff -plotrelief raised
        dui item moveto Aflowset inpor 2240 270
    } else {
        $::Aflow_demo_graph element configure line_espresso_de1_explanation_chart_flow -ydata espresso_de1_explanation_chart_flow_2x
        $::Aflow_demo_graph axis configure x -color #5a5d75
        $::Aflow_demo_graph axis configure y -color #18c37e
        $::Aflow_demo_graph axis configure y2 -hide 0
        $::Aflow_demo_graph grid configure -color #ddd
        $::Aflow_demo_graph configure -plotbackground #f8f8f8 -background #fff -plotrelief raised
        dui item moveto Aflowset inpor 2186 270

    }
}

proc demo_graph { {context {}} } {
	::plugins::A_Flow::prep
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
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
        
        # Filling and soaking
        #array set props [lindex $::settings(advanced_shot) 0]
        set sp $::Aflow_soaking_pressure
        set sp_b [expr {$sp*0.93}]
        set sp_a [expr {$sp*0.7}]
        set ff $::Aflow_filling_flow
        espresso_de1_explanation_chart_pressure append {0.0 0.0 $sp_a $sp_b $sp $sp}  
        espresso_de1_explanation_chart_flow append {$ff $ff $ff $ff 0.1 0.1}
        espresso_de1_explanation_chart_elapsed append {0.008 0.994 2.03 3.015 4 15}
        espresso_de1_explanation_chart_elapsed_flow append {0.008 0.994 2.03 3.015 4 15}
        
        set filling_temperature $::Aflow_filling_temperature
        foreach _ [espresso_de1_explanation_chart_pressure range 0 end] {
            espresso_de1_explanation_chart_temperature append $filling_temperature
            espresso_de1_explanation_chart_temperature_10 append [expr {$filling_temperature / 10.0}]
        }

        # pressure ramp up and down
        set pf $::Aflow_pouring_flow        
        if {$::ramp_down_enabled} { 
            set pf_2 [expr {$pf*2}]
        } else {
            set pf_2 $pf
        }
        set pp $::Aflow_pouring_pressure
        set pp_a [expr {$pp*0.5}]
        array set ramp_up [lindex $::settings(advanced_shot) 2]
        array set ramp_down [lindex $::settings(advanced_shot) 3]
        set ramp_up_end [round_to_integer [expr {15 + $ramp_up(seconds)}]]
        set ramp_down_end [expr {$ramp_up_end + $ramp_down(seconds)}]

        set ramp_start_time 15.1
        set time_array {}
        set time $ramp_start_time
        while {$time < $ramp_up_end} {
            lappend time_array $time
            set time [expr {$time + 0.5}]
        }
        
        foreach i $time_array {
            set linear_pressure [expr {$sp + ($pp - $sp) * ($i - $ramp_start_time) / ($ramp_up_end - $ramp_start_time)}]
            set linear_flow [expr {0.1 + ($pf_2 - 0.1) * ($i - $ramp_start_time) / ($ramp_up_end - $ramp_start_time)}]

            espresso_de1_explanation_chart_pressure append $linear_pressure
            espresso_de1_explanation_chart_flow append $linear_flow
            espresso_de1_explanation_chart_elapsed append $i
            espresso_de1_explanation_chart_elapsed_flow append $i        
            espresso_de1_explanation_chart_temperature append $::Aflow_pouring_temperature
            espresso_de1_explanation_chart_temperature_10 append [expr {$::Aflow_pouring_temperature / 10.0}]
        }

        if {$::ramp_down_enabled} {
            set time_array {}
            set time $ramp_up_end
            while {$time < $ramp_down_end} {
                lappend time_array $time
                set time [expr {$time + 0.5}]
            }

            foreach i $time_array {
                set linear_pressure [expr {$pp - ($pp - $::Aflow_ramp_down_pressure) * ($i - $ramp_up_end) / ($ramp_down_end - $ramp_up_end)}]
                set linear_flow [expr {$pf_2 - ($pf_2 - $pf) * ($i - $ramp_up_end) / ($ramp_down_end - $ramp_up_end)}]

                espresso_de1_explanation_chart_pressure append $linear_pressure
                espresso_de1_explanation_chart_flow append $linear_flow
                espresso_de1_explanation_chart_elapsed append $i
                espresso_de1_explanation_chart_elapsed_flow append $i   
                espresso_de1_explanation_chart_temperature append $::Aflow_pouring_temperature
                espresso_de1_explanation_chart_temperature_10 append [expr {$::Aflow_pouring_temperature / 10.0}]
            }
        }

        # final flow pouring 
        array set props [lindex $::settings(advanced_shot) 5]
        if {$::settings(final_desired_shot_volume_advanced) > 0 && $::settings(final_desired_shot_volume_advanced) < $::settings(final_desired_shot_weight_advanced)} {
            set shotendtime [expr {$::settings(final_desired_shot_volume_advanced) / $pf + 16}]
        } else {
            set shotendtime [expr {$::settings(final_desired_shot_weight_advanced) / $pf + 16}]
        }
        if {$shotendtime > $ramp_down_end} {
            set flow_ramp_start [expr {$ramp_down_end + 0.1}]
            espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
            espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
            espresso_de1_explanation_chart_pressure append $pp
            espresso_de1_explanation_chart_flow append $pf
            espresso_de1_explanation_chart_elapsed append $flow_ramp_start
            espresso_de1_explanation_chart_elapsed_flow append $flow_ramp_start

            espresso_de1_explanation_chart_pressure append $pp
            espresso_de1_explanation_chart_flow append [expr {(([ifexists props(flow)] - $pf) / [ifexists props(seconds)]) * ($shotendtime - $flow_ramp_start) + $pf}]
            espresso_de1_explanation_chart_temperature append [ifexists props(temperature)]
            espresso_de1_explanation_chart_temperature_10 append [expr {[ifexists props(temperature)] / 10.0}]
            espresso_de1_explanation_chart_elapsed append $shotendtime
            espresso_de1_explanation_chart_elapsed_flow append $shotendtime
        }
        

        foreach f [espresso_de1_explanation_chart_flow range 0 end] {
            espresso_de1_explanation_chart_flow_2x append [expr {2.0 * $f}]
        }
        dui item moveto Aflowset inpoc [expr {1130 + (1120 * 15 / (0.01 + $shotendtime))}] 270
        dui item moveto Aflowset inpoi [expr {1140 + ((1160 + (1120 * 15 / (0.01 + $shotendtime)) - 1170) / 2)}] 270
        if {$::settings(A_Flow_graph_style) == "Insight"} {
            set xcord 2240
        } else {
            set xcord 2186
        }
        dui item moveto Aflowset inpop [expr {$xcord - (($xcord - (1130 + (1120 * 15 / (0.01 + $shotendtime)))) / 2)}] 270

	}
}

proc reset_button_canvas {} {
    dui item config Aflowinfo dose_bg -outline #e9e9ed
    dui item config Aflowinfo infuse_temp_bg -outline #e9e9ed
    dui item config Aflowinfo infuse_pressure_bg -outline #e9e9ed
    dui item config Aflowinfo infuse_stop_bg -outline #e9e9ed
    dui item config Aflowinfo pour_temp_bg -outline #e9e9ed
    dui item config Aflowinfo pour_limits_bg -outline #e9e9ed
    dui item config Aflowinfo pour_stop_bg -outline #e9e9ed

    dui item config Aflowinfo info_intro -state hidden
    dui item config Aflowinfo info_dose -state hidden
    dui item config Aflowinfo info_infuse_temp -state hidden
    dui item config Aflowinfo info_infuse_pressure -state hidden
    dui item config Aflowinfo info_infuse_stop -state hidden
    dui item config Aflowinfo info_pour_temp -state hidden
    dui item config Aflowinfo info_pour_limits -state hidden
    dui item config Aflowinfo info_pour_stop -state hidden

    dui item show Aflowinfo dose_info_button*
    dui item show Aflowinfo infuse_temp_info_button*
    dui item show Aflowinfo infuse_pressure_info_button*
    dui item show Aflowinfo infuse_stop_info_button*
    dui item show Aflowinfo pour_temp_info_button*
    dui item show Aflowinfo pour_limits_info_button*
    dui item show Aflowinfo pour_stop_info_button*

    set ::plugins::A_Flow::info " "
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
dui add canvas_item rect $page_name 1657 1050 2265 1400 -fill #fff -width 2 -outline #e9e9ed -tags pour_limits_bg
dui add canvas_item rect $page_name 2271 1050 2493 1400 -fill #fff -width 2 -outline #e9e9ed -tags pour_stop_bg

dui add canvas_item rect $page_name 920 1386 1260 1436 -fill #ededfa -width 0 -outline #e9e9ed
dui add canvas_item rect $page_name 1757 1386 2165 1436 -fill #ededfa -width 0 -outline #e9e9ed
dui add canvas_item rect $page_name 2296 1386 2473 1436 -fill #ededfa -width 0 -outline #e9e9ed

### Settings
dui add variable $page_name 400 520 -justify center -anchor center -font [dui font get $font 16] -fill #ff574a -textvariable {$::Aflow_message}
dui add variable $page_name 1000 170 -justify center -anchor center -font [dui font get $font 12] -fill #ff9421 -textvariable {[::plugins::A_Flow::tap_to_update]}

dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -tags info_intro -text $::plugins::A_Flow::info_intro
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_dose -text $::plugins::A_Flow::info_dose
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_temp -text $::plugins::A_Flow::info_infuse_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_pressure -text $::plugins::A_Flow::info_infuse_pressure
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_stop -text $::plugins::A_Flow::info_infuse_stop
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_temp -text $::plugins::A_Flow::info_pour_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_limits -text $::plugins::A_Flow::info_pour_limits
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_stop -text $::plugins::A_Flow::info_pour_stop
dui add dtext $page_info 1280 900 -justify center -anchor center -font [dui font get $font 16] -fill #d7d9e6 -text {Tap within this info window to exit}

dui add dtext $page_name 1092 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text [translate {infuse until either}]
dui add dtext $page_name 1964 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text [translate {pouring settings}]
dui add dtext $page_name 2384 1410 -justify center -anchor center -font [dui font get $font 13] -fill #a7a9b6 -text [translate {stop at}]

dui add dtext $page_name 170 1010 -justify center -anchor center -font [dui font get $font 20] -fill #d7d9e6 -text {D O S E}
dui add dtext $page_name 170 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {weight}]
dui add variable $page_name 170 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_weight_measurement $::settings(grinder_dose_weight)]}

dui add dtext $page_name 825 1010 -justify center -anchor center -font [dui font get $font 24] -fill #d7d9e6 -text {I N F U S E}
dui add dtext $page_name 430 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {temperature}]
dui add variable $page_name 430 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_temperature_setting $::Aflow_filling_temperature]}
dui add dtext $page_name 670 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {pressure}]
dui add variable $page_name 680 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_pressure_measurement $::Aflow_soaking_pressure]}
dui add dtext $page_name 910 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {time}]
dui add dtext $page_name 1090 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {volume}]
dui add dtext $page_name 1270 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {weight}]
dui add variable $page_name 910 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::A_Flow::format_seconds $::Aflow_soaking_seconds]}
dui add variable $page_name 1090 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_stop_at_volume_measurement $::Aflow_soaking_volume]}
dui add variable $page_name 1270 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::A_Flow::format_SAW $::Aflow_soaking_weight]}

dui add dtext $page_name 1960 1010 -justify center -anchor center -font [dui font get $font 24] -fill #d7d9e6 -text {P O U R}
dui add variable $page_name 2290 1010 -justify center -anchor center -font [dui font get $font 20] -fill #b7b9c6 -textvariable {[::plugins::A_Flow::extraction_ratio]}
dui add dtext $page_name 1540 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {temperature}]
dui add dtext $page_name 1780 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {flow}]
dui add dtext $page_name 1960 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {pressure}]
dui add variable $page_name 1540 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_temperature_setting $::Aflow_pouring_temperature]}
dui add variable $page_name 1780 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_flow_measurement $::Aflow_pouring_flow]}
dui add variable $page_name 1970 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_pressure_measurement $::Aflow_pouring_pressure]}
dui add dtext $page_name 2140 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {time}]
dui add dtext $page_name 2380 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {weight}]
dui add variable $page_name 2140 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::A_Flow::format_seconds $::Aflow_ramp_updown_seconds]}
dui add variable $page_name 2380 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::plugins::A_Flow::format_weight_measurement $::settings(final_desired_shot_weight_advanced)]}

# Bean weight
dui add dbutton $page_name 100 1050 \
    -bwidth 150 -bheight 200 \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(grinder_dose_weight) [expr {$::settings(grinder_dose_weight) + 0.1}]
        set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    }
dui add dbutton $page_name 100 1250 \
    -bwidth 150 -bheight 200 \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(grinder_dose_weight) [expr {$::settings(grinder_dose_weight) - 0.1}]
        if {$::settings(grinder_dose_weight) < 0} {set ::settings(grinder_dose_weight) 0}
        set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    }

# Fill Temperature
dui add dbutton $page_name 340 1050 \
    -bwidth 180 -bheight 200 -tags fill_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_filling_temperature [round_to_integer [expr {$::Aflow_filling_temperature + 1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 340 1250 \
    -bwidth 180 -bheight 200 -tags fill_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_filling_temperature [round_to_integer [expr {$::Aflow_filling_temperature - 1}]]
        if {$::Aflow_filling_temperature < 80} {set ::Aflow_filling_temperature 80}
        ::plugins::A_Flow::update_A-Flow
    }

# Soaking flow pressure
dui add dbutton $page_name 580 1050 \
    -bwidth 180 -bheight 200 -tags soak_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_pressure [round_to_one_digits [expr {$::Aflow_soaking_pressure + 0.1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 580 1250 \
    -bwidth 180 -bheight 200 -tags soak_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_pressure [round_to_one_digits [expr {$::Aflow_soaking_pressure - 0.1}]]
        if {$::Aflow_soaking_pressure < 0.4} {set ::Aflow_soaking_pressure 0.4}
        ::plugins::A_Flow::update_A-Flow
    }

# Move on buttons
dui add dbutton $page_name 820 1050 \
    -bwidth 180 -bheight 200 -tags soak_seconds_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_seconds [round_to_integer [expr {$::Aflow_soaking_seconds + 1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 820 1250 \
    -bwidth 180 -bheight 200 -tags soak_seconds_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_seconds [round_to_integer [expr {$::Aflow_soaking_seconds - 1}]]
        if {$::Aflow_soaking_seconds < 0} {set :::Aflow_soaking_seconds 0}
        ::plugins::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1000 1050 \
    -bwidth 180 -bheight 200 -tags soak_volume_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_volume [round_to_integer [expr {$::Aflow_soaking_volume + 1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1000 1250 \
    -bwidth 180 -bheight 200 -tags soak_volume_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_volume [round_to_integer [expr {$::Aflow_soaking_volume - 1}]]
        if {$::Aflow_soaking_volume < 0} {set ::Aflow_soaking_volume 0}
        ::plugins::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1180 1050 \
    -bwidth 180 -bheight 200 -tags soak_weight_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_weight [round_to_one_digits [expr {$::Aflow_soaking_weight + 0.2}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1180 1250 \
    -bwidth 180 -bheight 200 -tags soak_weight_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_weight [round_to_one_digits [expr {$::Aflow_soaking_weight - 0.2}]]
        if {$::Aflow_soaking_weight < 0} {set ::Aflow_soaking_weight 0}
        ::plugins::A_Flow::update_A-Flow
    }

# pour buttons
dui add dbutton $page_name 1450 1050 \
    -bwidth 180 -bheight 200 -tags pour_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_temperature [round_to_integer [expr {$::Aflow_pouring_temperature + 1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1450 1250 \
    -bwidth 180 -bheight 200 -tags pour_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_temperature [round_to_integer [expr {$::Aflow_pouring_temperature - 1}]]
        if {$::Aflow_pouring_temperature < 0} {set ::Aflow_pouring_temperature 0}
        ::plugins::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1690 1050 \
    -bwidth 180 -bheight 200 -tags pouring_flow_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_flow [round_to_one_digits [expr {$::Aflow_pouring_flow + 0.1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1690 1250 \
    -bwidth 180 -bheight 200 -tags pouring_flow_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_flow [round_to_one_digits [expr {$::Aflow_pouring_flow - 0.1}]]
        if {$::Aflow_pouring_flow < 0.1} {set ::Aflow_pouring_flow 0.1}
        ::plugins::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1870 1050 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_pressure [round_to_one_digits [expr {$::Aflow_pouring_pressure + 0.1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1870 1250 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_pressure [round_to_one_digits [expr {$::Aflow_pouring_pressure - 0.1}]]
        if {$::Aflow_pouring_pressure < 0} {set ::Aflow_pouring_pressure 0}
        ::plugins::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 2050 1050 \
    -bwidth 180 -bheight 200 -tags SAV_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds + 1}]]
        ::plugins::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 2050 1250 \
    -bwidth 180 -bheight 200 -tags SAV_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds - 1}]]
        if {$::Aflow_ramp_updown_seconds < 0} {set ::Aflow_ramp_updown_seconds 0}
        ::plugins::A_Flow::update_A-Flow
    }

# stop at weight
dui add dbutton $page_name 2290 1050 \
    -bwidth 180 -bheight 200 -tags SAW_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) + 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::A_Flow::demo_graph
    }
dui add dbutton $page_name 2290 1250 \
    -bwidth 180 -bheight 200 -tags SAW_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) - 1}]]
        range_check_shot_variables
        profile_has_changed_set
        ::plugins::A_Flow::demo_graph
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
        ::plugins::A_Flow::save_A-Flow_profile
    }

dui add dtext $page_set 110 700 -justify center -anchor nw -font [dui font get $font 16] -fill $font_colour -text {A-Flow / }
add_de1_widget $page_set entry 270 690  {
    set ::globals(widget_aflow_save_as) $widget
    bind $widget <Return> { say [translate {save}] $::settings(sound_button_in)
    ::plugins::A_Flow::save_A-Flow_profile; hide_android_keyboard}
    bind $widget <Leave> hide_android_keyboard
} -width 18 -font Helv_8  -borderwidth 1 -bg #fbfaff  -foreground #4e85f4 -textvariable ::AFlow_name -relief flat  -highlightthickness 1 -highlightcolor #000000

# Add toggle widget below the existing widget
# toggle pressure ramp down on/off
dui add dtext $page_set 230 835 -justify center -anchor nw -font [dui font get $font 16] -fill $font_colour -text [translate {Ramp down}]
dui add dtoggle $page_set 100 830 -variable ::ramp_down_enabled -orient horizontal

proc ramp_down_toggle {} {
    if {$::ramp_down_enabled} {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds * 2}]]
    } else {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds / 2}]]
    }
    ::plugins::A_Flow::update_A-Flow
}

dui add dbutton $page_name 100 830 \
    -command {
        set ::ramp_down_enabled [expr {!$::ramp_down_enabled}]
        ::plugins::A_Flow::ramp_down_toggle
    }

# toggle pressure flow ramp up/down
dui add dtext $page_set 630 835 -justify center -anchor nw -font [dui font get $font 16] -fill $font_colour -text [translate {Flow up}]
dui add dtoggle $page_set 500 830 -variable ::flow_extraction_up -orient horizontal

dui add dbutton $page_name 500 830 \
    -command {
        set ::flow_extraction_up [expr {!$::flow_extraction_up}]
        ::plugins::A_Flow::update_A-Flow
    }

### Graph
dui add dtext $page_set 1140 270  -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {| <}
dui add dtext $page_set 2240 270 -tags inpor -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> |}
dui add dtext $page_set 1300 270 -tags inpoi -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Infuse}
dui add dtext $page_set 1840 270 -tags inpop -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Pour}
dui add dtext $page_set 1470 270 -tags inpoc -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> | <}

add_de1_widget "Aflowset" graph 1030 290 {
    set ::Aflow_demo_graph $widget
    ::plugins::A_Flow::demo_graph;

		$widget element create line_espresso_de1_explanation_chart_pressure -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_flow -xdata espresso_de1_explanation_chart_elapsed_flow -ydata espresso_de1_explanation_chart_flow  -label "" -linewidth [rescale_x_skin 12] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_temp -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_temperature_10  -label "" -linewidth [rescale_x_skin 10] -color #ff888c  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;


    $widget axis configure x -color #ddd -tickfont Helv_7 -min 0.0;
    $widget axis configure y -color #18c37e -tickfont Helv_7 -min 0.0 -max $::de1(max_pressure) -subdivisions 5 -majorticks {0  2  4  6  8  10  12}  -hide 0;
    $widget axis configure y2 -color #4e85f4 -tickfont Helv_7 -min 0.0 -max 6 -subdivisions 2 -majorticks {0  1  2  3  4  5  6} -hide 1;
    $widget grid configure -color #555
} -plotbackground #1e1e1e -width [rescale_x_skin 1250] -height [rescale_y_skin 590] -borderwidth 1 -background #1e1e1e -plotrelief flat
::plugins::A_Flow::select_flow_curve

dui add dbutton $page_set 2300 400 \
    -bwidth 200 -bheight 200 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label {showing} -label_font [dui font get $font 12] -label_justify center -label_anchor center -label_fill $font_colour -label_pos {0.5 0.28} \
    -label1variable {$::settings(A_Flow_graph_style)} -label1_justify center -label1_anchor center -label1_font [dui font get $font 16] -label1_fill $font_colour -label1_pos {0.5 0.5} \
    -label2 {style} -label2_font [dui font get $font 12] -label2_justify center -label2_anchor center -label2_fill $font_colour -label2_pos {0.5 0.72} \
    -command {
        ::plugins::A_Flow::toggle_graph
        ::plugins::A_Flow::demo_graph
    }

### Version credit
dui add variable $page_name 40 1570 -justify left -anchor w -font [dui font get "Font Awesome 5 Pro-Regular-400" 14] -fill #bbb -textvariable {A-Flow $::plugins::A_Flow::version - by $::plugins::A_Flow::author}

#### Info Page
### Info button
dui add dbutton $page_set 90 300 \
    -bwidth 160 -bheight 160 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "\uf129" -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 20] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        page_show Aflowinfo
    }
dui add dbutton $page_info 30 220 \
    -bwidth 2500 -bheight 710 \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        page_show Aflowset
    }

#####
dui add dbutton $page_info 55 1050 \
    -bwidth 230 -bheight 350 -tags dose_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_dose -state normal
        dui item config Aflowinfo dose_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo dose_info_button*

    }
dui add dbutton $page_info 315 1050 \
    -bwidth 230 -bheight 350 -tags infuse_temp_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_temp -state normal
        dui item config Aflowinfo infuse_temp_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo infuse_temp_info_button*
    }
dui add dbutton $page_info 545 1050 \
    -bwidth 230 -bheight 350 -tags infuse_pressure_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_pressure -state normal
        dui item config Aflowinfo infuse_pressure_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo infuse_pressure_info_button*
    }
dui add dbutton $page_info 795 1050 \
    -bwidth 600 -bheight 350 -tags infuse_stop_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_stop -state normal
        dui item config Aflowinfo infuse_stop_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo infuse_stop_info_button*
    }

dui add dbutton $page_info 1425 1050 \
    -bwidth 230 -bheight 350 -tags pour_temp_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_temp -state normal
        dui item config Aflowinfo pour_temp_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo pour_temp_info_button*
    }
dui add dbutton $page_info 1650 1050 \
    -bwidth 410 -bheight 350 -tags pour_limits_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_limits -state normal
        dui item config Aflowinfo pour_limits_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo pour_limits_info_button*
    }
dui add dbutton $page_info 2085 1050 \
    -bwidth 600 -bheight 350 -tags pour_stop_info_button \
    -command {
        ::plugins::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_stop -state normal
        dui item config Aflowinfo pour_stop_bg -outline $::plugins::A_Flow::info_colour
        dui item hide Aflowinfo pour_stop_info_button*
    }

############### Adapted navigation buttons from original settings
### tabs
add_de1_text $page_name 380 100 -text [translate "PRESETS"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_variable $page_name 1010 80 -text "" -font $settings_tab_font -fill "#2d3046"  -justify "center" -anchor "center" -textvariable {[setting_profile_type_to_text]}
add_de1_variable $page_name 1010 130 -text "" -font Helv_7 -fill "#2d3046"  -justify "center" -anchor "center" -textvariable {[wrapped_profile_title]}
add_de1_text $page_name 1650 100 -text [translate "MACHINE"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_text $page_name 2270 100 -text [translate "APP"] -font $settings_tab_font -fill "#7f879a" -anchor "center"
add_de1_button $page_set {after 500 update_de1_explanation_chart; say [translate {settings}] $::settings(sound_button_in); set_next_page off "settings_1"; page_show off; set ::settings(active_settings_tab) "settings_1"; set_profiles_scrollbar_dimensions} 0 0 641 188
add_de1_button $page_name {say [translate {save}] $::settings(sound_button_in); set ::settings(original_profile_title) $::settings(profile_title); if {$::settings(profile_has_changed) == 1} { borg toast [translate "Saved"]; save_profile; ::plugins::A_Flow::demo_graph} } 642 0 1277 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_3; page_show settings_3; scheduler_feature_hide_show_refresh; set ::settings(active_settings_tab) "settings_3"} 1278 0 1904 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_4; page_show settings_4; set ::settings(active_settings_tab) "settings_4"; set_ble_scrollbar_dimensions; set_ble_scale_scrollbar_dimensions} 1905 0 2560 188

########## setting_2 tab button
dui add dbutton "settings_1 settings_3 settings_4" 642 0 1277 188 \
            -bwidth 635 -bheight 188 \
            -labelvariable {} -label_font [dui font get $font 12] -label_fill $font_colour -label_pos {0.5 0.5} \
            -command {
            set title_test [string range [ifexists ::settings(profile_title)] 0 7]
            if {$title_test == "A-Flow /" } {
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
    -bwidth 200 -bheight 200 -tags aflow_profile_button -initial_state hidden \
    -command {
        set title_test [string range [ifexists ::settings(profile_title)] 0 7]
        if {$title_test == "A-Flow /" } {
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
    if {$title_test == "A-Flow /" } {
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
        if {$title_test == "A-Flow /" } {
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

################ Mod external code
rename ::update_de1_plus_advanced_explanation_chart ::update_de1_plus_advanced_explanation_chart_non_aflow
proc ::update_de1_plus_advanced_explanation_chart {} {
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        ::plugins::A_Flow::demo_graph
    } else {
        ::update_de1_plus_advanced_explanation_chart_non_aflow
    }
}

rename ::setting_profile_type_to_text ::setting_profile_type_to_text_non_aflow
proc ::setting_profile_type_to_text {} {
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" || $title_test == "D-Flow /"} {
        dui item show settings_1 aflow_profile_button*
        $::globals(widget_profile_name_to_save) configure -state disabled
    } else {
        dui item hide settings_1 aflow_profile_button*
        $::globals(widget_profile_name_to_save) configure -state normal
    }
    if {$title_test == "A-Flow /" } {
        setting_profile_type_to_text_non_aflow
        .can coords $::tab1_profile_label [lindex [.can coords $::tab1_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab2_profile_label [lindex [.can coords $::tab2_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab3_profile_label [lindex [.can coords $::tab3_profile_label] 0] [rescale_y_skin 80]
        .can coords $::tab4_profile_label [lindex [.can coords $::tab4_profile_label] 0] [rescale_y_skin 80]
        $::preview_graph_advanced axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "A-Flow"] -titlefont Helv_8 -titlecolor #5a5d75;
        return "A-FLOW"
    } else {
        $::preview_graph_advanced axis configure y -color #5a5d75 -tickfont Helv_6 -min 0.0 -max 12 -majorticks {1 2 3 4 5 6 7 8 9 10 11 12} -title [translate "Advanced"] -titlefont Helv_8 -titlecolor #5a5d75;
        setting_profile_type_to_text_non_aflow
    }
}

rename ::wrapped_profile_title ::wrapped_profile_title_non_aflow
proc ::wrapped_profile_title {} {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        set dft [string range [ifexists ::settings(profile_title)] 9 35]
        return $dft
    } else {
        wrapped_profile_title_non_aflow
    }
}

}