# A-Flow

Profile plugin based on D-Flow. It's a mix of D-Flow and Adaptive (for medium roasts) profile plus some adaptions. 

## How to use it?
1. copy `A_Flow_Espresso_Profile` folder to plugin directory of DE1
2. update D-Flow: copy plugin.tcl to `D_Flow_Espresso_Profile` in plugin directory of DE1
3. restart the DE1 app and enable the plugin

## A-Flow Profile

The profile has the following steps and default values: 
1. **Fill**: fill step with 8 ml/s flow until pressure reaches 3 bar
2. **Infuse**: pressurized infusion or soaking step with 3 bar pressure and move on by weight at 6g
3. **Pressure Up**: ramp pressure up to 9 bar and move on by flow over. The flow threshold depends on `Ramp down`. If enabled the threshold is 2 times the pouring flow. If disabled the threshold is equal to the pouring flow  
4. **Pressure Decline**: ramp pressure down to 1 bar and move on by flow under pouring flow. Step can be disabled by `Ramp down` toggle 
5. **Flow Start**: not an active step, only used to have a nicer graph when using the default advanced profile editor
6. **Flow Extraction**: flow extraction with increasing (`Flow up` enabled) or decreasing flow ramp 

### Pouring Parameters
Infuse parameters are not changed compared to D-Flow. Only the fill step is different with 8ml/s flow. 

* **Temperature**: temperature during pouring steps (3-6)
* **Pressure**: peak of pressure up step and max pressure during flow extraction
* **Flow**: 
  * `Ramp down` enabled: start flow of flow extraction and max flow during pressure up (move on when over 2 times set flow value)
  * `Ramp down` disabled: start flow of flow extraction equals max flow during pressure up
* **Time**: 
  * `Ramp down` enabled: duration of pressure up and pressure decline steps, if value is odd then pressure decline is 1 sec longer 
  * `Ramp down` disabled: duration of pressure up step
* **Weight**: final stop by weight threshold

### Comparison
The profile is a mix of D-Flow and Adaptive (for medium roasts) profile plus an optional pressure decline step, similar to LRv2 and other lever profiles.
The main purpose is an easy way to try the different approaches to find the best fit for the beans at hand. 

* if the `Ramp down` toggle is disabled, it's quite similar Adaptive (for medium roasts) with more focus on a controlled flow. The pouring flow defines the move on threshold for pressure up step.  
* enabling the `Ramp down` allows a higher pressure to increase puck resistence and a low pressure flow extraction at the end
* setting the pouring time to 0s/off lead to a profile similar to D-Flow, but with an increasing or decreasing flow 

