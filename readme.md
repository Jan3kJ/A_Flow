# A-Flow

Profile plugin based on D-Flow. It's a mix of D-Flow and Adaptive (for medium roasts) profile plus some adaptions. 

## How to use it?
Enable the plugin in the DE1 app: settings -> APP -> Extensions -> select A-Flow

To get started, select one of the default profiles default-light, default-medium, default-dark depending on your beans. 

Some basic guidelines to dial in the profile:
1. Grind fine enough to reach a pressure between 6 and 9 bar peak at extraction phase
2. For light and medium roasts, the infusion step should be 20-30 seconds. For dark roasts, the infusion step should be 10-20 seconds. If the step is longer your grind is probably too fine
3. Adjust the pouring flow depending on the taste: light roasts tend to need a higher flow rate, dark roasts a lower flow rate
4. Adjust target weight: decrease if to bitter, increase if to sour
5. Toggle `Flow up` and see if that improves the taste 
6. Adjust the temperature: increase for more sweetness and intensity, decrease for more decent flavors and to avoid burned flavores 

There is no golden rule, that fits all beans. Stop optimizing when it tastes good and enjoy! 

## A-Flow Profile

The profile has the following steps and default values: 
1. **Fill**: fill step with 8 ml/s flow until pressure reaches 3 bar
2. **Infuse**: pressurized infusion or soaking step with 3 bar pressure and move on by weight
3. **Pressure Up**: ramp pressure up to set pouring pressure and move on by flow over. The flow threshold depends on `Ramp down`. If enabled the threshold is 2 times the pouring flow. If disabled the threshold is equal to the pouring flow  
4. **Pressure Decline**: ramp pressure down to 1 bar and move on by flow under pouring flow. Step can be disabled by `Ramp down` toggle 
5. **Flow Start**: only active if pouring time is set to 0s, to enable a fast transition to flow extraction. 
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

The screenshot below shows flow, pressure and time parameter and points to the parts, which are defined by these parameters.

![a_flow.jpg](img%2Fa_flow.jpg)

### Default Profiles
The plugin comes with 5 default profiles, which can be used as a starting point. 
* **default-light**: profile for light roasts
* **default-medium**: profile for medium roasts
* **default-dark**: profile for dark roasts
* **default-like-dflow**: similar to D-Flow, but with an increasing or decreasing flow 
* **default-very-dark**: profile with `Ramp down` enabled, which works best with dark roasts

### Comparison
The profile is a mix of D-Flow and Adaptive (for medium roasts) profile plus an optional pressure decline step, similar to LRv2 and other lever profiles.
The main purpose is an easy way to try the different approaches to find the best fit for the beans at hand. 

* if the `Ramp down` toggle is disabled, it's quite similar Adaptive (for medium roasts) with more focus on a controlled flow. The pouring flow defines the move on threshold for pressure up step.  
* enabling the `Ramp down` allows a higher pressure to increase puck resistence and a low pressure flow extraction at the end
* setting the pouring time to 0s/off lead to a profile similar to D-Flow, but with an increasing or decreasing flow 

