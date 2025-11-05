### A-Flow profile editor by Janek, forked from Damian Brakel's D-Flow profile ###

proc show_editor { args } {
    set_next_page off Aflowset
    page_show off
}

set plugin_name "A_Flow"
variable author "Janek"
variable description "A-Flow is a simple to use advanced profile based on D-Flow and 'adaptive for medium roasts' profile. For more infomation click on the info button on the settings pane."
variable version 2.0

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
Infuse temperature allows you to emulate various machine where the group temperature 
may start cooler than the water being added. See pour temperature for information 
on how temperature effects extraction and taste.
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
For best consistancy it's recommend to use weight. However, in some cases like very dark roasts or 
low pressure infusion, time is the better option.  

A longer infusion increases body, but it also reduces puck resistance during the pour.
Which means you need to grind finer to maintain the same pour pressure/flow rates.
Finer grinds also reduce body and can increase bitterness, so the aim is to find a balance for your prefered taste.

A target weight of 4g to 6g is a good starting range to experiment with.

When the "2nd fill" is activated, an additional filling step is added before the pouring phase. 
This is helpful at low infusion pressure to refill the filter before the pressure ramp starts.
}

set info_pour_temp {
Higher temperature helps extract the soluble compounds, however it also increases
harsher tasts that may exist too.

Higher temperatures usually results in lower body with more pronounced acidity, wine, fruity, vegetal flavours.
Lower temperatures often work better with slower extraction rates.
}

set info_pour_limits {
A-Flow uses a pressure ramp to slowly increase the extraction flow after infusion phase. 
If "ramp down" enabled, followed by a pressure decline step to reach a defined extraction flow rate. 
Final extraction flow is either slowly increase or decreasing, depending on "flow up".

Time defines the duration of the pressure ramp (up and down). It's not the duration of the pouring phase.
It's possible to skip the pressure increase and decline by setting the time to 0, 
which lead to a similar profile as D-Flow. 

Increasing pressure will shift taste from wine like to a more syrupy texture, it also
shifts tastes from clear delicate flavours to more muddled flavours.

Faster flows can increase clarity in ligher roasts and may help reduce channeling.
Slower flow can help increase intensity and body in darker than light roasts.
}

set info_pour_stop "

The extraction ratio   (Dose : Extraction)   is shown above the setting dial.
Increasing the extraction ratio will shift the taste from

Sour  >  Sweet  >  Bitter

The ideal extraction ratio can vary between beans, water alkalinity, puck prep methods 
and how evenly the puck is extracted. You should adjusted this setting for your taste.
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
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); back_to_previous_page; fill_advanced_profile_steps_listbox; restore_espresso_chart; LRv2_preview; DSx_graph_restore; save_settings_to_de1; fill_profiles_listbox; fill_extensions_listbox; refresh_DSx_temperature; ::A_Flow::prep} 1505 1430 2015 1600
    } else {
    add_de1_button $page_set {if {[ifexists ::profiles_hide_mode] == 1} { unset -nocomplain ::profiles_hide_mode; fill_profiles_listbox }; array unset ::settings {\*}; array set ::settings [array get ::settings_backup]; update_de1_explanation_chart; fill_skin_listbox; profile_has_changed_set_colors; say [translate {Cancel}] $::settings(sound_button_in); set_next_page off off; page_show off; fill_advanced_profile_steps_listbox; restore_espresso_chart; save_settings_to_de1; fill_profiles_listbox ; fill_extensions_listbox; ::A_Flow::prep} 1505 1430 2015 1600
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


proc set_profile_index {} {
    if {[llength $::settings(advanced_shot)] > 8} {
        set ::index_pre_filling 0
        set ::index_filling 1
        set ::index_soaking 2
        set ::index_2nd_fill 3
        set ::index_pause 4
        set ::index_ramp_up 5
        set ::index_ramp_down 6
        set ::index_pouring_start 7
        set ::index_pouring 8
    } else {
        set ::index_filling 0
        set ::index_soaking 1
        set ::index_ramp_up 2
        set ::index_ramp_down 3
        set ::index_pouring_start 4
        set ::index_pouring 5
    }
}


### Check / write profile
proc prep { args } {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        set_profile_index
        array set filling [lindex $::settings(advanced_shot) $::index_filling]
        array set soaking [lindex $::settings(advanced_shot) $::index_soaking]
        array set ramp_up [lindex $::settings(advanced_shot) $::index_ramp_up]
        array set ramp_down [lindex $::settings(advanced_shot) $::index_ramp_down]
        array set pouring_start [lindex $::settings(advanced_shot) $::index_pouring_start]
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
        # check if ramp down is enabled
        set ::ramp_down_enabled false
        if {$ramp_down(seconds) > 0} {
            set ::ramp_down_enabled true
        } 
        # check if flow up is enabled
        array set pouring [lindex $::settings(advanced_shot) $::index_pouring]
        set ::flow_extraction_up false
        if {$pouring(flow) > $::Aflow_pouring_flow} {
            set ::flow_extraction_up true
        } 

        set ::2nd_fill_step false
        if {[llength $::settings(advanced_shot)] > 8} {
            # check if second fill step is enabled
            array set pause [lindex $::settings(advanced_shot) $::index_pause]
            if {$pause(seconds) > 0} {
                set ::2nd_fill_step true
            }
        }

        update_2nd_fill
        update_flow_up
        update_ramp_down
        
    }
}


proc update_A-Flow {} {
    set_profile_index
    array set filling [lindex $::settings(advanced_shot) $::index_filling]
    array set soaking [lindex $::settings(advanced_shot) $::index_soaking]
    array set ramp_up [lindex $::settings(advanced_shot) $::index_ramp_up]
    array set ramp_down [lindex $::settings(advanced_shot) $::index_ramp_down]
    array set pouring_start [lindex $::settings(advanced_shot) $::index_pouring_start]
    array set pouring [lindex $::settings(advanced_shot) $::index_pouring]
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

    # check if profile has new format and add new steps pre_filling and 2nd_fill if not present
    if {[llength $::settings(advanced_shot)] > 8} {
        # new profile
        # read pre_filling and 2nd_fill from profile
        array set pre_filling [lindex $::settings(advanced_shot) $::index_pre_filling]
        array set 2nd_fill [lindex $::settings(advanced_shot) $::index_2nd_fill]
        array set pause [lindex $::settings(advanced_shot) $::index_pause]
    } else {
        # update old profiles -> add new steps pre_filling and 2nd_fill
        # prefill can be removed in future, if "skip first step" bug is fixed
        array set pre_filling { 
            exit_if 0 
            flow 8.0 
            volume 100 
            max_flow_or_pressure_range 0.6 
            transition fast 
            popup {} 
            exit_flow_under 0 
            temperature 95
            weight 0.0 
            name {Pre Fill} 
            pressure 3.0 
            sensor coffee 
            pump flow 
            exit_type pressure_over 
            exit_flow_over 6 
            exit_pressure_over 3.00 
            max_flow_or_pressure 8.0 
            exit_pressure_under 0 
            seconds 1.00
        }
        array set 2nd_fill {
            exit_if 1 
            flow 8.0 
            volume 100 
            max_flow_or_pressure_range 0.6 
            transition fast 
            popup {} 
            exit_flow_under 0 
            temperature 95 
            weight 0 
            name {2nd Fill} 
            pressure 0 
            pump flow 
            sensor coffee 
            exit_type pressure_over 
            exit_flow_over 6 
            exit_pressure_over 2.50 
            max_flow_or_pressure 3.0 
            seconds 0.00 
            exit_pressure_under 0
        }
        array set pause {
            exit_if 1  
            flow 6.0  
            volume 100  
            max_flow_or_pressure_range 0.6  
            transition fast  
            popup {}  
            exit_flow_under 1.00  
            temperature 95  
            weight 0  
            name {Pause}
            pressure 1.0  
            pump pressure 
            sensor coffee  
            exit_type flow_under  
            exit_flow_over 6  
            exit_pressure_over 0.0  
            max_flow_or_pressure 1.0  
            seconds 0.00  
            exit_pressure_under 0  
        }
    }

    # check if second fill step is enabled
    if {$::2nd_fill_step} {
        set pause(seconds) 15
        set pause(temperature) $::Aflow_pouring_temperature
        set 2nd_fill(seconds) 15
        set 2nd_fill(temperature) $::Aflow_pouring_temperature
    } else {
        set pause(seconds) 0
        set 2nd_fill(seconds) 0
    }
    # set temperature for pre_filling step
    set pre_filling(temperature) $::Aflow_filling_temperature

    # create new profile
    set newprofile {}
    lappend newprofile [array get pre_filling]
    lappend newprofile [array get filling]
    lappend newprofile [array get soaking]
    lappend newprofile [array get 2nd_fill]
    lappend newprofile [array get pause]
    lappend newprofile [array get ramp_up]
    lappend newprofile [array get ramp_down]
    lappend newprofile [array get pouring_start]
    lappend newprofile [array get pouring]    
    set ::settings(advanced_shot) $newprofile
    set_profile_index
    range_check_shot_variables
    profile_has_changed_set
    ::A_Flow::demo_graph
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
        ::A_Flow::demo_graph

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
        set c [::A_Flow::format_SAW $::Aflow_soaking_weight]
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
    ::A_Flow::select_flow_curve
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
	set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        ::A_Flow::prep
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
        set sp $::Aflow_soaking_pressure
        set sp_b [expr {$sp*0.93}]
        set sp_a [expr {$sp*0.7}]
        set ff $::Aflow_filling_flow
        set flow_ramp_start 0.1
        set pressure_ramp_start $sp

        if {$::2nd_fill_step} {
            espresso_de1_explanation_chart_pressure append {0.0 0.0 $sp_a $sp_b $sp $sp $sp 2 1}  
            espresso_de1_explanation_chart_flow append {$ff $ff $ff $ff 0.1 0.1 8 8 0.8}
            espresso_de1_explanation_chart_elapsed append {0.008 0.994 2.03 3.015 4 11.9 12 13.5 15}
            espresso_de1_explanation_chart_elapsed_flow append {0.008 0.994 2.03 3.015 4 11.9 12 13.5 15}
            # set start values for pressure/flow ramp
            set pressure_ramp_start 1  
            set flow_ramp_start 0.8
        } else {
            espresso_de1_explanation_chart_pressure append {0.0 0.0 $sp_a $sp_b $sp $sp}  
            espresso_de1_explanation_chart_flow append {$ff $ff $ff $ff 0.1 0.1}
            espresso_de1_explanation_chart_elapsed append {0.008 0.994 2.03 3.015 4 15}
            espresso_de1_explanation_chart_elapsed_flow append {0.008 0.994 2.03 3.015 4 15}
        }
        
        
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
        array set ramp_up [lindex $::settings(advanced_shot) $::index_ramp_up]
        array set ramp_down [lindex $::settings(advanced_shot) $::index_ramp_down]
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
            set linear_pressure [expr {$pressure_ramp_start + ($pp - $pressure_ramp_start) * ($i - $ramp_start_time) / ($ramp_up_end - $ramp_start_time)}]
            set linear_flow [expr {$flow_ramp_start + ($pf_2 - $flow_ramp_start) * ($i - $ramp_start_time) / ($ramp_up_end - $ramp_start_time)}]

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
        array set props [lindex $::settings(advanced_shot) $::index_pouring]
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

    set ::A_Flow::info " "
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
dui add variable $page_name 1000 170 -justify center -anchor center -font [dui font get $font 12] -fill #ff9421 -textvariable {[::A_Flow::tap_to_update]}

dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -tags info_intro -text $::A_Flow::info_intro
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_dose -text $::A_Flow::info_dose
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_temp -text $::A_Flow::info_infuse_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_pressure -text $::A_Flow::info_infuse_pressure
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_infuse_stop -text $::A_Flow::info_infuse_stop
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_temp -text $::A_Flow::info_pour_temp
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_limits -text $::A_Flow::info_pour_limits
dui add dtext $page_info 1280 520 -justify center -anchor center -font [dui font get $font 16] -fill $info_colour -initial_state hidden -tags info_pour_stop -text $::A_Flow::info_pour_stop
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
dui add variable $page_name 910 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::A_Flow::format_seconds $::Aflow_soaking_seconds]}
dui add variable $page_name 1090 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_stop_at_volume_measurement $::Aflow_soaking_volume]}
dui add variable $page_name 1270 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::A_Flow::format_SAW $::Aflow_soaking_weight]}

dui add dtext $page_name 1960 1010 -justify center -anchor center -font [dui font get $font 24] -fill #d7d9e6 -text {P O U R}
dui add variable $page_name 2290 1010 -justify center -anchor center -font [dui font get $font 20] -fill #b7b9c6 -textvariable {[::A_Flow::extraction_ratio]}
dui add dtext $page_name 1540 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {temperature}]
dui add dtext $page_name 1780 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {flow}]
dui add dtext $page_name 1960 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {pressure}]
dui add variable $page_name 1540 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_temperature_setting $::Aflow_pouring_temperature]}
dui add variable $page_name 1780 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_flow_measurement $::Aflow_pouring_flow]}
dui add variable $page_name 1970 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[return_pressure_measurement $::Aflow_pouring_pressure]}
dui add dtext $page_name 2140 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {time}]
dui add dtext $page_name 2380 1080 -justify center -anchor center -font [dui font get $font 12] -fill $font_colour -text [translate {weight}]
dui add variable $page_name 2140 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::A_Flow::format_seconds $::Aflow_ramp_updown_seconds]}
dui add variable $page_name 2380 1250 -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -textvariable {[::A_Flow::format_weight_measurement $::settings(final_desired_shot_weight_advanced)]}

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
        dui page open_dialog dui_number_editor ::settings(grinder_dose_weight) \
        -n_decimals 1 -min 0 -max 30 -default $::settings(grinder_dose_weight) \
        -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Dose weight"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values dose] \
        -return_callback "aflow_callback_bean_weight callback_after_adv_profile_data_entry dose"
    }

# Fill Temperature
dui add dbutton $page_name 340 1050 \
    -bwidth 180 -bheight 200 -tags fill_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_filling_temperature [round_to_integer [expr {$::Aflow_filling_temperature + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 340 1250 \
    -bwidth 180 -bheight 200 -tags fill_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_filling_temperature [round_to_integer [expr {$::Aflow_filling_temperature - 1}]]
        if {$::Aflow_filling_temperature < 80} {set ::Aflow_filling_temperature 80}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 340 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_filling_temperature \
        -n_decimals 0 -min 0 -max 105 -default $::Aflow_filling_temperature \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Infuse Temperature"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values temp] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry temp"
    }

# Soaking flow pressure
dui add dbutton $page_name 580 1050 \
    -bwidth 180 -bheight 200 -tags soak_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_pressure [round_to_one_digits [expr {$::Aflow_soaking_pressure + 0.1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 580 1250 \
    -bwidth 180 -bheight 200 -tags soak_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_pressure [round_to_one_digits [expr {$::Aflow_soaking_pressure - 0.1}]]
        if {$::Aflow_soaking_pressure < 0} {set ::Aflow_soaking_pressure 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 580 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_soaking_pressure \
        -n_decimals 1 -min 0 -max $::de1(maxpressure) -default $::Aflow_soaking_pressure \
        -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Infuse Pressure"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values pressure] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry pressure"
    }

# Move on buttons
dui add dbutton $page_name 820 1050 \
    -bwidth 180 -bheight 200 -tags soak_seconds_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_seconds [round_to_integer [expr {$::Aflow_soaking_seconds + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 820 1250 \
    -bwidth 180 -bheight 200 -tags soak_seconds_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_seconds [round_to_integer [expr {$::Aflow_soaking_seconds - 1}]]
        if {$::Aflow_soaking_seconds < 0} {set :::Aflow_soaking_seconds 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 820 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_soaking_seconds \
        -n_decimals 0 -min 0 -max 1000 -default $::Aflow_soaking_seconds \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Infuse Time (seconds)"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values time] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry time"
    }

dui add dbutton $page_name 1000 1050 \
    -bwidth 180 -bheight 200 -tags soak_volume_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_volume [round_to_integer [expr {$::Aflow_soaking_volume + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1000 1250 \
    -bwidth 180 -bheight 200 -tags soak_volume_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_volume [round_to_integer [expr {$::Aflow_soaking_volume - 1}]]
        if {$::Aflow_soaking_volume < 0} {set ::Aflow_soaking_volume 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1000 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_soaking_volume \
        -n_decimals 0 -min 0 -max 1000 -default $::Aflow_soaking_volume \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Infuse Volume"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values volume] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry volume"
    }

dui add dbutton $page_name 1180 1050 \
    -bwidth 180 -bheight 200 -tags soak_weight_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_weight [round_to_one_digits [expr {$::Aflow_soaking_weight + 0.2}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1180 1250 \
    -bwidth 180 -bheight 200 -tags soak_weight_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_soaking_weight [round_to_one_digits [expr {$::Aflow_soaking_weight - 0.2}]]
        if {$::Aflow_soaking_weight < 0} {set ::Aflow_soaking_weight 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1180 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_soaking_weight \
        -n_decimals 1 -min 0 -max 1000 -default $::Aflow_soaking_weight \
        -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Infuse Weight"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values weight] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry weight"
    }

# pour buttons
dui add dbutton $page_name 1450 1050 \
    -bwidth 180 -bheight 200 -tags pour_temp_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_temperature [round_to_integer [expr {$::Aflow_pouring_temperature + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1450 1250 \
    -bwidth 180 -bheight 200 -tags pour_temp_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_temperature [round_to_integer [expr {$::Aflow_pouring_temperature - 1}]]
        if {$::Aflow_pouring_temperature < 0} {set ::Aflow_pouring_temperature 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1450 1210 \
    -bwidth 1450 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_pouring_temperature \
        -n_decimals 0 -min 0 -max 105 -default $::Aflow_pouring_temperature \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Pour Temperature"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values temp] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry temp"
    }

dui add dbutton $page_name 1690 1050 \
    -bwidth 180 -bheight 200 -tags pouring_flow_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_flow [round_to_one_digits [expr {$::Aflow_pouring_flow + 0.1}]]
        ::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1690 1250 \
    -bwidth 180 -bheight 200 -tags pouring_flow_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_flow [round_to_one_digits [expr {$::Aflow_pouring_flow - 0.1}]]
        if {$::Aflow_pouring_flow < 0.1} {set ::Aflow_pouring_flow 0.1}
        ::A_Flow::update_A-Flow
    }
dui add dbutton $page_name 1690 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_pouring_flow \
        -n_decimals 1 -min 0.1 -max $::de1(max_flowrate_v11) -default $::Aflow_pouring_flow \
        -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Pour Flow rate"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values flow] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry flow"
    }

dui add dbutton $page_name 1870 1050 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_pressure [round_to_one_digits [expr {$::Aflow_pouring_pressure + 0.1}]]
        ::A_Flow::update_A-Flow
    }
    
dui add dbutton $page_name 1870 1250 \
    -bwidth 180 -bheight 200 -tags pouring_pressure_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_pouring_pressure [round_to_one_digits [expr {$::Aflow_pouring_pressure - 0.1}]]
        if {$::Aflow_pouring_pressure < 0} {set ::Aflow_pouring_pressure 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 1870 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_pouring_pressure \
        -n_decimals 1 -min 0 -max $::de1(maxpressure) -default $::Aflow_pouring_pressure \
        -smallincrement 0.1 -bigincrement 1 -use_biginc 1 -page_title [translate "Maximum Pour Pressure"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values pressure] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry pressure"
    }

dui add dbutton $page_name 2050 1050 \
    -bwidth 180 -bheight 200 -tags SAV_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 2050 1250 \
    -bwidth 180 -bheight 200 -tags SAV_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds - 1}]]
        if {$::Aflow_ramp_updown_seconds < 0} {set ::Aflow_ramp_updown_seconds 0}
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 2050 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::Aflow_ramp_updown_seconds \
        -n_decimals 0 -min 0 -max 1000 -default $::Aflow_ramp_updown_seconds \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Pour Volume"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values sav] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry sav"
    }

# stop at weight
dui add dbutton $page_name 2290 1050 \
    -bwidth 180 -bheight 200 -tags SAW_up \
    -label \uf106 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) + 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 2290 1250 \
    -bwidth 180 -bheight 200 -tags SAW_down \
    -label \uf107 -label_font [dui font get "Font Awesome 5 Pro-Regular-400" 18] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::settings(final_desired_shot_weight_advanced) [round_to_integer [expr {$::settings(final_desired_shot_weight_advanced) - 1}]]
        ::A_Flow::update_A-Flow
    }

dui add dbutton $page_name 2290 1210 \
    -bwidth 150 -bheight 80 \
    -command {
        dui page open_dialog dui_number_editor ::settings(final_desired_shot_weight_advanced) \
        -n_decimals 0 -min 0 -max 1000 -default $::settings(final_desired_shot_weight_advanced) \
        -smallincrement 1 -bigincrement 10 -use_biginc 1 -page_title [translate "Maximum Pour Weight"] \
        -previous_values [::dui::pages::dui_number_editor::get_previous_values saw] \
        -return_callback "aflow_callback_update callback_after_adv_profile_data_entry saw"
    }


proc ::aflow_callback_update {nextproc context data} {
    ::A_Flow::update_A-Flow
    ::dui::pages::dui_number_editor::save_previous_value $nextproc $context $data
}


proc ::aflow_callback_bean_weight {nextproc context data} {
    set ::DSx_settings(bean_weight) $::settings(grinder_dose_weight)
    ::dui::pages::dui_number_editor::save_previous_value $nextproc $context $data
}


### Save as
dui add dbutton $page_set 85 555 \
    -bwidth 760 -bheight 230 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -command {
        # do nothing to avoid warning
    }

dui add dbutton $page_set 100 570 \
    -bwidth 730 -bheight 100 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label "save as" -label_font [dui font get $font 16] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        say [translate {save}] $::settings(sound_button_in)
        ::A_Flow::save_A-Flow_profile
    }

dui add dtext $page_set 110 700 -justify center -anchor nw -font [dui font get $font 16] -fill $font_colour -text {A-Flow / }
add_de1_widget $page_set entry 270 690  {
    set ::globals(widget_aflow_save_as) $widget
    bind $widget <Return> { say [translate {save}] $::settings(sound_button_in)
    ::A_Flow::save_A-Flow_profile; hide_android_keyboard}
    bind $widget <Leave> hide_android_keyboard
} -width 18 -font Helv_8  -borderwidth 1 -bg #fbfaff  -foreground #4e85f4 -textvariable ::AFlow_name -relief flat  -highlightthickness 1 -highlightcolor #000000

# Add toggle widgets below the existing widget
# Initialize variables if not set
if {![info exists ::ramp_down_enabled]} { set ::ramp_down_enabled false }
if {![info exists ::flow_extraction_up]} { set ::flow_extraction_up false }
if {![info exists ::2nd_fill_step]} { set ::2nd_fill_step false }

# Toggle button styles
set toggle_width 240
set toggle_height 80
set active_bg #4e85f4
set active_width 4


proc update_ramp_down {} {
    if {$::ramp_down_enabled} {
        dui item config $::A_Flow::page_set ramp_down_toggle -outline $::A_Flow::active_bg -width $::A_Flow::active_width
    } else {
        dui item config $::A_Flow::page_set ramp_down_toggle -width $::A_Flow::button_outline_width -outline $::A_Flow::button_outline_colour
    }
}

proc update_flow_up {} {
    if {$::flow_extraction_up} {
        dui item config $::A_Flow::page_set flow_up_toggle -outline $::A_Flow::active_bg -width $::A_Flow::active_width
    } else {
        dui item config $::A_Flow::page_set flow_up_toggle -width $::A_Flow::button_outline_width -outline $::A_Flow::button_outline_colour
    }
}

proc update_2nd_fill {} {
    if {$::2nd_fill_step} {
        dui item config $::A_Flow::page_set 2nd_fill_toggle -outline $::A_Flow::active_bg -width $::A_Flow::active_width
    } else {
        dui item config $::A_Flow::page_set 2nd_fill_toggle -width $::A_Flow::button_outline_width -outline $::A_Flow::button_outline_colour
    }
}

# Ramp down toggle
dui add dbutton $page_set 85 820 \
    -bwidth $toggle_width -bheight $toggle_height -tags ramp_down_toggle \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label [translate {Ramp Down}] -label_font [dui font get $font 16] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::ramp_down_enabled [expr {!$::ramp_down_enabled}]
            if {$::ramp_down_enabled} {
            set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds * 2}]]
        } else {
            set ::Aflow_ramp_updown_seconds [round_to_integer [expr {$::Aflow_ramp_updown_seconds / 2}]]
        }
        ::A_Flow::update_ramp_down
        ::A_Flow::update_A-Flow
    }

# Flow up toggle
dui add dbutton $page_set 345 820 \
    -bwidth $toggle_width -bheight $toggle_height -tags flow_up_toggle \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label [translate {Flow Up}] -label_font [dui font get $font 16] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::flow_extraction_up [expr {!$::flow_extraction_up}]
        ::A_Flow::update_flow_up
        ::A_Flow::update_A-Flow
    }

# Second fill step toggle
dui add dbutton $page_set 605 820 \
    -bwidth $toggle_width -bheight $toggle_height -tags 2nd_fill_toggle \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label [translate {2nd Fill}] -label_font [dui font get $font 16] -label_fill $icon_colour -label_pos {0.5 0.5} \
    -command {
        set ::2nd_fill_step [expr {!$::2nd_fill_step}]
        ::A_Flow::update_2nd_fill
        ::A_Flow::update_A-Flow
    }

### Graph
dui add dtext $page_set 1140 270  -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {| <}
dui add dtext $page_set 2240 270 -tags inpor -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> |}
dui add dtext $page_set 1300 270 -tags inpoi -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Infuse}
dui add dtext $page_set 1840 270 -tags inpop -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {Pour}
dui add dtext $page_set 1470 270 -tags inpoc -justify center -anchor center -font [dui font get $font 16] -fill $font_colour -text {> | <}

add_de1_widget "Aflowset" graph 1030 290 {
    set ::Aflow_demo_graph $widget
    ::A_Flow::demo_graph;

		$widget element create line_espresso_de1_explanation_chart_pressure -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_pressure  -label "" -linewidth [rescale_x_skin 10] -color #47e098  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_flow -xdata espresso_de1_explanation_chart_elapsed_flow -ydata espresso_de1_explanation_chart_flow  -label "" -linewidth [rescale_x_skin 12] -color #98c5ff  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;
		$widget element create line_espresso_de1_explanation_chart_temp -xdata espresso_de1_explanation_chart_elapsed -ydata espresso_de1_explanation_chart_temperature_10  -label "" -linewidth [rescale_x_skin 10] -color #ff888c  -smooth $::settings(preview_graph_smoothing_technique) -pixels 0;


    $widget axis configure x -color #ddd -tickfont Helv_7 -min 0.0;
    $widget axis configure y -color #18c37e -tickfont Helv_7 -min 0.0 -max $::de1(max_pressure) -subdivisions 5 -majorticks {0  2  4  6  8  10  12}  -hide 0;
    $widget axis configure y2 -color #4e85f4 -tickfont Helv_7 -min 0.0 -max 6 -subdivisions 2 -majorticks {0  1  2  3  4  5  6} -hide 1;
    $widget grid configure -color #555
} -plotbackground #1e1e1e -width [rescale_x_skin 1250] -height [rescale_y_skin 590] -borderwidth 1 -background #1e1e1e -plotrelief flat
::A_Flow::select_flow_curve

dui add dbutton $page_set 2300 400 \
    -bwidth 200 -bheight 200 \
    -shape outline -width $button_outline_width -outline $button_outline_colour \
    -label {showing} -label_font [dui font get $font 12] -label_justify center -label_anchor center -label_fill $font_colour -label_pos {0.5 0.28} \
    -label1variable {$::settings(A_Flow_graph_style)} -label1_justify center -label1_anchor center -label1_font [dui font get $font 16] -label1_fill $font_colour -label1_pos {0.5 0.5} \
    -label2 {style} -label2_font [dui font get $font 12] -label2_justify center -label2_anchor center -label2_fill $font_colour -label2_pos {0.5 0.72} \
    -command {
        ::A_Flow::toggle_graph
        ::A_Flow::demo_graph
    }

### Version credit
dui add variable $page_name 40 1570 -justify left -anchor w -font [dui font get "Font Awesome 5 Pro-Regular-400" 14] -fill #bbb -textvariable {A-Flow $::A_Flow::version - by $::A_Flow::author}

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
        ::A_Flow::reset_button_canvas
        page_show Aflowset
    }

#####
dui add dbutton $page_info 55 1050 \
    -bwidth 230 -bheight 350 -tags dose_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_dose -state normal
        dui item config Aflowinfo dose_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo dose_info_button*

    }

dui add dbutton $page_info 315 1050 \
    -bwidth 230 -bheight 350 -tags infuse_temp_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_temp -state normal
        dui item config Aflowinfo infuse_temp_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo infuse_temp_info_button*
    }

dui add dbutton $page_info 545 1050 \
    -bwidth 230 -bheight 350 -tags infuse_pressure_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_pressure -state normal
        dui item config Aflowinfo infuse_pressure_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo infuse_pressure_info_button*
    }

dui add dbutton $page_info 795 1050 \
    -bwidth 600 -bheight 350 -tags infuse_stop_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_infuse_stop -state normal
        dui item config Aflowinfo infuse_stop_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo infuse_stop_info_button*
    }

dui add dbutton $page_info 1425 1050 \
    -bwidth 230 -bheight 350 -tags pour_temp_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_temp -state normal
        dui item config Aflowinfo pour_temp_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo pour_temp_info_button*
    }

dui add dbutton $page_info 1650 1050 \
    -bwidth 410 -bheight 350 -tags pour_limits_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_limits -state normal
        dui item config Aflowinfo pour_limits_bg -outline $::A_Flow::info_colour
        dui item hide Aflowinfo pour_limits_info_button*
    }
    
dui add dbutton $page_info 2085 1050 \
    -bwidth 600 -bheight 350 -tags pour_stop_info_button \
    -command {
        ::A_Flow::reset_button_canvas
        dui item config Aflowinfo info_intro -state hidden
        dui item config Aflowinfo info_pour_stop -state normal
        dui item config Aflowinfo pour_stop_bg -outline $::A_Flow::info_colour
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
add_de1_button $page_name {say [translate {save}] $::settings(sound_button_in); set ::settings(original_profile_title) $::settings(profile_title); if {$::settings(profile_has_changed) == 1} { borg toast [translate "Saved"]; save_profile; ::A_Flow::demo_graph} } 642 0 1277 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_3; page_show settings_3; scheduler_feature_hide_show_refresh; set ::settings(active_settings_tab) "settings_3"} 1278 0 1904 188
add_de1_button $page_set {say [translate {settings}] $::settings(sound_button_in); set_next_page off settings_4; page_show settings_4; set ::settings(active_settings_tab) "settings_4"; set_ble_scrollbar_dimensions; set_ble_scale_scrollbar_dimensions} 1905 0 2560 188

### ok/cancel buttons
add_de1_text $page_set 2275 1520 -text [translate "Ok"] -font $botton_button_font -fill "#FFFFFF" -anchor "center"
add_de1_text $page_set 1760 1520 -text [translate "Cancel"] -font $botton_button_font -fill "#FFFFFF" -anchor "center"
add_de1_button $page_set {exit_profile_editor ok} 2016 1430 2560 1600
add_de1_button $page_set {exit_profile_editor cancel} 1505 1430 2015 1600

proc set_editor {args} {
    set title_test [string range [ifexists ::settings(profile_title)] 0 7]
    if {$title_test == "A-Flow /" } {
        set ::settings(profile_editor) A_Flow
    }
}

trace add execution select_profile {leave} ::A_Flow::set_editor

rename ::plugins::list ::plugins::list_aflow
proc ::plugins::list {args} {
    set list [lsearch -all -inline -not -exact [::plugins::list_aflow] "A_Flow"]
    return $list
}
