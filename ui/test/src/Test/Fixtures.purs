module Test.Fixtures where

signeFixture :: String
signeFixture = """
  {
    "date_code": "20221006",
    "definition": {
      "description": "Hue Gradient Signe floor lamp (black)",
      "exposes": [
        {
          "features": [
            {
              "access": 7,
              "description": "On/off state of this light",
              "label": "State",
              "name": "state",
              "property": "state",
              "type": "binary",
              "value_off": "OFF",
              "value_on": "ON",
              "value_toggle": "TOGGLE"
            },
            {
              "access": 7,
              "description": "Brightness of this light",
              "label": "Brightness",
              "name": "brightness",
              "property": "brightness",
              "type": "numeric",
              "value_max": 254,
              "value_min": 0
            },
            {
              "access": 7,
              "description": "Color temperature of this light",
              "label": "Color temp",
              "name": "color_temp",
              "presets": [
                {
                  "description": "Coolest temperature supported",
                  "name": "coolest",
                  "value": 153
                },
                {
                  "description": "Cool temperature (250 mireds / 4000 Kelvin)",
                  "name": "cool",
                  "value": 250
                },
                {
                  "description": "Neutral temperature (370 mireds / 2700 Kelvin)",
                  "name": "neutral",
                  "value": 370
                },
                {
                  "description": "Warm temperature (454 mireds / 2200 Kelvin)",
                  "name": "warm",
                  "value": 454
                },
                {
                  "description": "Warmest temperature supported",
                  "name": "warmest",
                  "value": 500
                }
              ],
              "property": "color_temp",
              "type": "numeric",
              "unit": "mired",
              "value_max": 500,
              "value_min": 153
            },
            {
              "access": 7,
              "description": "Color temperature after cold power on of this light",
              "label": "Color temp startup",
              "name": "color_temp_startup",
              "presets": [
                {
                  "description": "Coolest temperature supported",
                  "name": "coolest",
                  "value": 153
                },
                {
                  "description": "Cool temperature (250 mireds / 4000 Kelvin)",
                  "name": "cool",
                  "value": 250
                },
                {
                  "description": "Neutral temperature (370 mireds / 2700 Kelvin)",
                  "name": "neutral",
                  "value": 370
                },
                {
                  "description": "Warm temperature (454 mireds / 2200 Kelvin)",
                  "name": "warm",
                  "value": 454
                },
                {
                  "description": "Warmest temperature supported",
                  "name": "warmest",
                  "value": 500
                },
                {
                  "description": "Restore previous color_temp on cold power on",
                  "name": "previous",
                  "value": 65535
                }
              ],
              "property": "color_temp_startup",
              "type": "numeric",
              "unit": "mired",
              "value_max": 500,
              "value_min": 153
            },
            {
              "access": 7,
              "description": "Color of this light in the CIE 1931 color space (x/y)",
              "features": [
                {
                  "access": 7,
                  "label": "X",
                  "name": "x",
                  "property": "x",
                  "type": "numeric"
                },
                {
                  "access": 7,
                  "label": "Y",
                  "name": "y",
                  "property": "y",
                  "type": "numeric"
                }
              ],
              "label": "Color (X/Y)",
              "name": "color_xy",
              "property": "color",
              "type": "composite"
            },
            {
              "access": 7,
              "description": "Color of this light expressed as hue/saturation",
              "features": [
                {
                  "access": 7,
                  "label": "Hue",
                  "name": "hue",
                  "property": "hue",
                  "type": "numeric"
                },
                {
                  "access": 7,
                  "label": "Saturation",
                  "name": "saturation",
                  "property": "saturation",
                  "type": "numeric"
                }
              ],
              "label": "Color (HS)",
              "name": "color_hs",
              "property": "color",
              "type": "composite"
            }
          ],
          "type": "light"
        },
        {
          "access": 7,
          "description": "Controls the behavior when the device is powered on after power loss",
          "label": "Power-on behavior",
          "name": "power_on_behavior",
          "property": "power_on_behavior",
          "type": "enum",
          "values": [
            "off",
            "on",
            "toggle",
            "previous"
          ]
        },
        {
          "access": 2,
          "label": "Gradient scene",
          "name": "gradient_scene",
          "property": "gradient_scene",
          "type": "enum",
          "values": [
            "blossom",
            "crocus",
            "precious",
            "narcissa",
            "beginnings",
            "first_light",
            "horizon",
            "valley_dawn",
            "sunflare",
            "emerald_flutter",
            "memento",
            "resplendent",
            "scarlet_dream",
            "lovebirds",
            "smitten",
            "glitz_and_glam",
            "promise",
            "ruby_romance",
            "city_of_love",
            "honolulu",
            "savanna_sunset",
            "golden_pond",
            "runy_glow",
            "tropical_twilight",
            "miami",
            "cancun",
            "rio",
            "chinatown",
            "ibiza",
            "osaka",
            "tokyo",
            "motown",
            "fairfax",
            "galaxy",
            "starlight",
            "blood moon",
            "artic_aurora",
            "moonlight",
            "nebula",
            "sundown",
            "blue_lagoon",
            "palm_beach",
            "lake_placid",
            "mountain_breeze",
            "lake_mist",
            "ocean_dawn",
            "frosty_dawn",
            "sunday_morning",
            "emerald_isle",
            "spring_blossom",
            "midsummer_sun",
            "autumn_gold",
            "spring_lake",
            "winter_mountain",
            "midwinter",
            "amber_bloom",
            "lily",
            "painted_sky",
            "winter_beauty",
            "orange_fields",
            "forest_adventure",
            "blue_planet",
            "soho",
            "vapor_wave",
            "magneto",
            "tyrell",
            "disturbia",
            "hal",
            "golden_star",
            "under_the_tree",
            "silent_night",
            "rosy_sparkle",
            "festive_fun",
            "colour_burst",
            "crystalline"
          ]
        },
        {
          "access": 7,
          "description": "List of RGB HEX colors",
          "item_type": {
            "access": 7,
            "description": "Color in RGB HEX format (eg #663399)",
            "label": "Hex",
            "name": "hex",
            "type": "text"
          },
          "label": "Gradient",
          "length_max": 9,
          "length_min": 1,
          "name": "gradient",
          "property": "gradient",
          "type": "list"
        },
        {
          "access": 2,
          "label": "Effect",
          "name": "effect",
          "property": "effect",
          "type": "enum",
          "values": [
            "blink",
            "breathe",
            "okay",
            "channel_change",
            "candle",
            "fireplace",
            "colorloop",
            "sunrise",
            "finish_effect",
            "stop_effect",
            "stop_hue_effect"
          ]
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "915005987601",
      "options": [
        {
          "access": 2,
          "description": "Controls the transition time (in seconds) of on/off, brightness, color temperature (if applicable) and color (if applicable) changes. Defaults to `0` (no transition).",
          "label": "Transition",
          "name": "transition",
          "property": "transition",
          "type": "numeric",
          "value_min": 0
        },
        {
          "access": 2,
          "description": "When enabled colors will be synced, e.g. if the light supports both color x/y and color temperature a conversion from color x/y to color temperature will be done when setting the x/y color (default true).",
          "label": "Color sync",
          "name": "color_sync",
          "property": "color_sync",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 2,
          "description": "State actions will also be published as 'action' when true (default false).",
          "label": "State action",
          "name": "state_action",
          "property": "state_action",
          "type": "binary",
          "value_off": false,
          "value_on": true
        }
      ],
      "supports_ota": true,
      "vendor": "Philips"
    },
    "disabled": false,
    "endpoints": {
      "11": {
        "bindings": [
          {
            "cluster": "manuSpecificPhilips2",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "genGroups",
            "genScenes",
            "genOnOff",
            "genLevelCtrl",
            "touchlink",
            "manuSpecificPhilips2",
            "lightingColorCtrl",
            "manuSpecificUbisysDimmerSetup"
          ],
          "output": [
            "genOta"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      },
      "242": {
        "bindings": [],
        "clusters": {
          "input": [],
          "output": [
            "greenPower"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      }
    },
    "friendly_name": "Basement Black Signe",
    "ieee_address": "0x001788010c52373e",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "Philips",
    "model_id": "915005987601",
    "network_address": 27893,
    "power_source": "Mains (single phase)",
    "software_build_id": "1.101.2",
    "supported": true,
    "type": "Router"
  }
"""

humiditySensorFixture :: String
humiditySensorFixture = """
{
    "date_code": "2022-01-27 08:40",
    "definition": {
      "description": "Temperature & humidity sensor",
      "exposes": [
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 1,
          "description": "Indicates if the battery of this device is almost empty",
          "label": "Battery low",
          "name": "battery_low",
          "property": "battery_low",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Measured temperature value",
          "label": "Temperature",
          "name": "temperature",
          "property": "temperature",
          "type": "numeric",
          "unit": "Â°C"
        },
        {
          "access": 1,
          "description": "Measured relative humidity",
          "label": "Humidity",
          "name": "humidity",
          "property": "humidity",
          "type": "numeric",
          "unit": "%"
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "HMSZB-110",
      "options": [
        {
          "access": 2,
          "description": "Number of digits after decimal point for temperature, takes into effect on next report of device.",
          "label": "Temperature precision",
          "name": "temperature_precision",
          "property": "temperature_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the temperature value (absolute offset), takes into effect on next report of device.",
          "label": "Temperature calibration",
          "name": "temperature_calibration",
          "property": "temperature_calibration",
          "type": "numeric"
        },
        {
          "access": 2,
          "description": "Number of digits after decimal point for humidity, takes into effect on next report of device.",
          "label": "Humidity precision",
          "name": "humidity_precision",
          "property": "humidity_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the humidity value (absolute offset), takes into effect on next report of device.",
          "label": "Humidity calibration",
          "name": "humidity_calibration",
          "property": "humidity_calibration",
          "type": "numeric"
        }
      ],
      "supports_ota": false,
      "vendor": "Develco"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [],
        "clusters": {
          "input": [
            "genIdentify",
            "genScenes",
            "genOnOff"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "38": {
        "bindings": [
          {
            "cluster": "genPollCtrl",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "msTemperatureMeasurement",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "msRelativeHumidity",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genPowerCfg",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "genPollCtrl",
            "msTemperatureMeasurement",
            "msRelativeHumidity"
          ],
          "output": [
            "genIdentify",
            "genTime",
            "genOta"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "measuredValue",
            "cluster": "msTemperatureMeasurement",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 10
          },
          {
            "attribute": "measuredValue",
            "cluster": "msRelativeHumidity",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 300
          },
          {
            "attribute": "batteryVoltage",
            "cluster": "genPowerCfg",
            "maximum_report_interval": 43200,
            "minimum_report_interval": 3600,
            "reportable_change": 100
          }
        ],
        "scenes": []
      }
    },
    "friendly_name": "Grow room temperature and humidity",
    "ieee_address": "0x0015bc0035001e14",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "frient A/S",
    "model_id": "HMSZB-110",
    "network_address": 7639,
    "power_source": "Battery",
    "software_build_id": "3.4.6",
    "supported": true,
    "type": "EndDevice"
  }
"""

motionSensorFixture :: String
motionSensorFixture = """
{
    "date_code": "20220329 07:28",
    "definition": {
      "description": "Motion sensor",
      "exposes": [
        {
          "access": 1,
          "description": "Indicates whether the device detected occupancy",
          "label": "Occupancy",
          "name": "occupancy",
          "property": "occupancy",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 7,
          "label": "Occupancy timeout",
          "name": "occupancy_timeout",
          "property": "occupancy_timeout",
          "type": "numeric",
          "unit": "second",
          "value_max": 65535,
          "value_min": 20
        },
        {
          "access": 1,
          "description": "Measured temperature value",
          "label": "Temperature",
          "name": "temperature",
          "property": "temperature",
          "type": "numeric",
          "unit": "Â°C"
        },
        {
          "access": 1,
          "description": "Measured illuminance in lux",
          "label": "Illuminance (lux)",
          "name": "illuminance_lux",
          "property": "illuminance_lux",
          "type": "numeric",
          "unit": "lx"
        },
        {
          "access": 1,
          "description": "Indicates whether the device is tampered",
          "label": "Tamper",
          "name": "tamper",
          "property": "tamper",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Indicates if the battery of this device is almost empty",
          "label": "Battery low",
          "name": "battery_low",
          "property": "battery_low",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 7,
          "description": "Control LED indicator usage.",
          "label": "Led control",
          "name": "led_control",
          "property": "led_control",
          "type": "enum",
          "values": [
            "off",
            "fault_only",
            "motion_only",
            "both"
          ]
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "MOSZB-140",
      "options": [
        {
          "access": 2,
          "description": "Number of digits after decimal point for temperature, takes into effect on next report of device.",
          "label": "Temperature precision",
          "name": "temperature_precision",
          "property": "temperature_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the temperature value (absolute offset), takes into effect on next report of device.",
          "label": "Temperature calibration",
          "name": "temperature_calibration",
          "property": "temperature_calibration",
          "type": "numeric"
        },
        {
          "access": 2,
          "description": "Calibrates the illuminance value (percentual offset), takes into effect on next report of device.",
          "label": "Illuminance calibration",
          "name": "illuminance_calibration",
          "property": "illuminance_calibration",
          "type": "numeric"
        },
        {
          "access": 2,
          "description": "Calibrates the illuminance_lux value (percentual offset), takes into effect on next report of device.",
          "label": "Illuminance lux calibration",
          "name": "illuminance_lux_calibration",
          "property": "illuminance_lux_calibration",
          "type": "numeric"
        }
      ],
      "supports_ota": true,
      "vendor": "Develco"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [],
        "clusters": {
          "input": [
            "genIdentify",
            "genScenes",
            "genOnOff"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "34": {
        "bindings": [],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msOccupancySensing"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "35": {
        "bindings": [
          {
            "cluster": "genPollCtrl",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genPowerCfg",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "genBinaryInput",
            "genPollCtrl",
            "ssIasZone"
          ],
          "output": [
            "genIdentify",
            "genTime",
            "genOta"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "batteryVoltage",
            "cluster": "genPowerCfg",
            "maximum_report_interval": 43200,
            "minimum_report_interval": 3600,
            "reportable_change": 100
          }
        ],
        "scenes": []
      },
      "38": {
        "bindings": [
          {
            "cluster": "msTemperatureMeasurement",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msTemperatureMeasurement"
          ],
          "output": []
        },
        "configured_reportings": [
          {
            "attribute": "measuredValue",
            "cluster": "msTemperatureMeasurement",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 100
          }
        ],
        "scenes": []
      },
      "39": {
        "bindings": [
          {
            "cluster": "msIlluminanceMeasurement",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msIlluminanceMeasurement"
          ],
          "output": []
        },
        "configured_reportings": [
          {
            "attribute": "measuredValue",
            "cluster": "msIlluminanceMeasurement",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 500
          }
        ],
        "scenes": []
      },
      "40": {
        "bindings": [],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msOccupancySensing"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "41": {
        "bindings": [],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msOccupancySensing"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      }
    },
    "friendly_name": "Basement stair top motion sensor",
    "ieee_address": "0x0015bc001a0270a6",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "frient A/S",
    "model_id": "MOSZB-140",
    "network_address": 56495,
    "power_source": "Battery",
    "software_build_id": "4.0.3",
    "supported": true,
    "type": "EndDevice"
  }
"""

controlsDeviceFixture :: String
controlsDeviceFixture = """
{
    "date_code": "NULL",
    "definition": {
      "description": "4 zone remote and dimmer",
      "exposes": [
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 1,
          "description": "Triggered action (e.g. a button click)",
          "label": "Action",
          "name": "action",
          "property": "action",
          "type": "enum",
          "values": [
            "brightness_move_up",
            "brightness_move_down",
            "brightness_stop",
            "on",
            "off",
            "recall_*"
          ]
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "SR-ZG9001K12-DIM-Z4",
      "options": [
        {
          "access": 2,
          "description": "Simulate a brightness value. If this device provides a brightness_move_up or brightness_move_down action it is possible to specify the update interval and delta. The action_brightness_delta indicates the delta for each interval. ",
          "features": [
            {
              "access": 2,
              "description": "Delta per interval, 20 by default",
              "label": "Delta",
              "name": "delta",
              "property": "delta",
              "type": "numeric",
              "value_min": 0
            },
            {
              "access": 2,
              "description": "Interval duration",
              "label": "Interval",
              "name": "interval",
              "property": "interval",
              "type": "numeric",
              "unit": "ms",
              "value_min": 0
            }
          ],
          "label": "Simulated brightness",
          "name": "simulated_brightness",
          "property": "simulated_brightness",
          "type": "composite"
        },
        {
          "access": 2,
          "description": "Set to false to disable the legacy integration (highly recommended), will change structure of the published payload (default true).",
          "label": "Legacy",
          "name": "legacy",
          "property": "legacy",
          "type": "binary",
          "value_off": false,
          "value_on": true
        }
      ],
      "supports_ota": false,
      "vendor": "Sunricher"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [
          {
            "cluster": "genOnOff",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genScenes",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "haDiagnostic",
            "touchlink"
          ],
          "output": [
            "genIdentify",
            "genGroups",
            "genScenes",
            "genOnOff",
            "genLevelCtrl",
            "genOta",
            "lightingColorCtrl",
            "touchlink"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      },
      "2": {
        "bindings": [
          {
            "cluster": "genOnOff",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "haDiagnostic",
            "touchlink"
          ],
          "output": [
            "genIdentify",
            "genGroups",
            "genScenes",
            "genOnOff",
            "genLevelCtrl",
            "genOta",
            "lightingColorCtrl",
            "touchlink"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      },
      "3": {
        "bindings": [
          {
            "cluster": "genOnOff",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "haDiagnostic",
            "touchlink"
          ],
          "output": [
            "genIdentify",
            "genGroups",
            "genScenes",
            "genOnOff",
            "genLevelCtrl",
            "genOta",
            "lightingColorCtrl",
            "touchlink"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      },
      "4": {
        "bindings": [
          {
            "cluster": "genOnOff",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "haDiagnostic",
            "touchlink"
          ],
          "output": [
            "genIdentify",
            "genGroups",
            "genScenes",
            "genOnOff",
            "genLevelCtrl",
            "genOta",
            "lightingColorCtrl",
            "touchlink"
          ]
        },
        "configured_reportings": [],
        "scenes": []
      }
    },
    "friendly_name": "Vesternet Zigbee Remote Control - Twelve Button",
    "ieee_address": "0xb4e3f9fffeec5e6a",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "Sunricher",
    "model_id": "ZGRC-KEY-013",
    "network_address": 17719,
    "power_source": "Battery",
    "software_build_id": "2.7.6_r25",
    "supported": true,
    "type": "EndDevice"
  }
"""

contactSensorFixture :: String
contactSensorFixture = """
{
    "date_code": "2020-09-08 14:34",
    "definition": {
      "description": "Window sensor",
      "exposes": [
        {
          "access": 1,
          "description": "Indicates if the contact is closed (= true) or open (= false)",
          "label": "Contact",
          "name": "contact",
          "property": "contact",
          "type": "binary",
          "value_off": true,
          "value_on": false
        },
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 1,
          "description": "Indicates if the battery of this device is almost empty",
          "label": "Battery low",
          "name": "battery_low",
          "property": "battery_low",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Indicates whether the device is tampered",
          "label": "Tamper",
          "name": "tamper",
          "property": "tamper",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Measured temperature value",
          "label": "Temperature",
          "name": "temperature",
          "property": "temperature",
          "type": "numeric",
          "unit": "Â°C"
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "WISZB-120",
      "options": [
        {
          "access": 2,
          "description": "Number of digits after decimal point for temperature, takes into effect on next report of device.",
          "label": "Temperature precision",
          "name": "temperature_precision",
          "property": "temperature_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the temperature value (absolute offset), takes into effect on next report of device.",
          "label": "Temperature calibration",
          "name": "temperature_calibration",
          "property": "temperature_calibration",
          "type": "numeric"
        }
      ],
      "supports_ota": true,
      "vendor": "Develco"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [],
        "clusters": {
          "input": [
            "genIdentify",
            "genScenes",
            "genOnOff"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "35": {
        "bindings": [
          {
            "cluster": "genPollCtrl",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genPowerCfg",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "genBinaryInput",
            "genPollCtrl",
            "ssIasZone"
          ],
          "output": [
            "genTime",
            "genOta"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "batteryVoltage",
            "cluster": "genPowerCfg",
            "maximum_report_interval": 62000,
            "minimum_report_interval": 3600,
            "reportable_change": 0
          }
        ],
        "scenes": []
      },
      "38": {
        "bindings": [
          {
            "cluster": "msTemperatureMeasurement",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genIdentify",
            "msTemperatureMeasurement"
          ],
          "output": [
            "genIdentify"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "measuredValue",
            "cluster": "msTemperatureMeasurement",
            "maximum_report_interval": 3600,
            "minimum_report_interval": 10,
            "reportable_change": 100
          }
        ],
        "scenes": []
      }
    },
    "friendly_name": "Front door entry sensor",
    "ieee_address": "0x0015bc001e00f658",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "frient A/S",
    "model_id": "WISZB-120",
    "network_address": 21753,
    "power_source": "Battery",
    "software_build_id": "3.4.19",
    "supported": true,
    "type": "EndDevice"
  }
"""

airQualitySensorFixture :: String
airQualitySensorFixture = """
{
    "date_code": "20210824 22:26",
    "definition": {
      "description": "Air quality sensor",
      "exposes": [
        {
          "access": 1,
          "description": "Measured VOC value",
          "label": "VOC",
          "name": "voc",
          "property": "voc",
          "type": "numeric",
          "unit": "Âµg/mÂ³"
        },
        {
          "access": 1,
          "description": "Measured temperature value",
          "label": "Temperature",
          "name": "temperature",
          "property": "temperature",
          "type": "numeric",
          "unit": "Â°C"
        },
        {
          "access": 1,
          "description": "Measured relative humidity",
          "label": "Humidity",
          "name": "humidity",
          "property": "humidity",
          "type": "numeric",
          "unit": "%"
        },
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 1,
          "description": "Indicates if the battery of this device is almost empty",
          "label": "Battery low",
          "name": "battery_low",
          "property": "battery_low",
          "type": "binary",
          "value_off": false,
          "value_on": true
        },
        {
          "access": 1,
          "description": "Measured air quality",
          "label": "Air quality",
          "name": "air_quality",
          "property": "air_quality",
          "type": "enum",
          "values": [
            "excellent",
            "good",
            "moderate",
            "poor",
            "unhealthy",
            "out_of_range",
            "unknown"
          ]
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "AQSZB-110",
      "options": [
        {
          "access": 2,
          "description": "Number of digits after decimal point for voc, takes into effect on next report of device.",
          "label": "Voc precision",
          "name": "voc_precision",
          "property": "voc_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the voc value (absolute offset), takes into effect on next report of device.",
          "label": "Voc calibration",
          "name": "voc_calibration",
          "property": "voc_calibration",
          "type": "numeric"
        },
        {
          "access": 2,
          "description": "Number of digits after decimal point for temperature, takes into effect on next report of device.",
          "label": "Temperature precision",
          "name": "temperature_precision",
          "property": "temperature_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the temperature value (absolute offset), takes into effect on next report of device.",
          "label": "Temperature calibration",
          "name": "temperature_calibration",
          "property": "temperature_calibration",
          "type": "numeric"
        },
        {
          "access": 2,
          "description": "Number of digits after decimal point for humidity, takes into effect on next report of device.",
          "label": "Humidity precision",
          "name": "humidity_precision",
          "property": "humidity_precision",
          "type": "numeric",
          "value_max": 3,
          "value_min": 0
        },
        {
          "access": 2,
          "description": "Calibrates the humidity value (absolute offset), takes into effect on next report of device.",
          "label": "Humidity calibration",
          "name": "humidity_calibration",
          "property": "humidity_calibration",
          "type": "numeric"
        }
      ],
      "supports_ota": true,
      "vendor": "Develco"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [],
        "clusters": {
          "input": [
            "genIdentify",
            "genScenes",
            "genOnOff"
          ],
          "output": []
        },
        "configured_reportings": [],
        "scenes": []
      },
      "38": {
        "bindings": [
          {
            "cluster": "genPollCtrl",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "develcoSpecificAirQuality",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "msTemperatureMeasurement",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "msRelativeHumidity",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genPowerCfg",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "genPollCtrl",
            "msTemperatureMeasurement",
            "msRelativeHumidity",
            "1070",
            "develcoSpecificAirQuality"
          ],
          "output": [
            "genIdentify",
            "genTime",
            "genOta"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "measuredValue",
            "cluster": "develcoSpecificAirQuality",
            "maximum_report_interval": 3600,
            "minimum_report_interval": 60,
            "reportable_change": 10
          },
          {
            "attribute": "measuredValue",
            "cluster": "msTemperatureMeasurement",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 10
          },
          {
            "attribute": "measuredValue",
            "cluster": "msRelativeHumidity",
            "maximum_report_interval": 600,
            "minimum_report_interval": 60,
            "reportable_change": 300
          },
          {
            "attribute": "batteryVoltage",
            "cluster": "genPowerCfg",
            "maximum_report_interval": 43200,
            "minimum_report_interval": 3600,
            "reportable_change": 100
          }
        ],
        "scenes": []
      }
    },
    "friendly_name": "Basement gym air quality sensor",
    "ieee_address": "0x0015bc0036000f69",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "frient A/S",
    "model_id": "AQSZB-110",
    "network_address": 6480,
    "power_source": "Battery",
    "software_build_id": "4.0.1",
    "supported": true,
    "type": "EndDevice"
  }
"""

windowCoveringFixture :: String
windowCoveringFixture = """
{
    "date_code": "20201028",
    "definition": {
      "description": "TREDANSEN cellular blind",
      "exposes": [
        {
          "features": [
            {
              "access": 3,
              "label": "State",
              "name": "state",
              "property": "state",
              "type": "enum",
              "values": [
                "OPEN",
                "CLOSE",
                "STOP"
              ]
            },
            {
              "access": 7,
              "description": "Position of this cover",
              "label": "Position",
              "name": "position",
              "property": "position",
              "type": "numeric",


              "unit": "%",
              "value_max": 100,
              "value_min": 0
            }
          ],
          "type": "cover"
        },
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported.",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        },
        {
          "access": 1,
          "description": "Link quality (signal strength)",
          "label": "Linkquality",
          "name": "linkquality",
          "property": "linkquality",
          "type": "numeric",
          "unit": "lqi",
          "value_max": 255,
          "value_min": 0
        }
      ],
      "model": "E2103",
      "options": [
        {
          "access": 2,
          "description": "Inverts the cover position, false: open=100,close=0, true: open=0,close=100 (default false).",
          "label": "Invert cover",
          "name": "invert_cover",
          "property": "invert_cover",
          "type": "binary",
          "value_off": false,
          "value_on": true
        }
      ],
      "supports_ota": true,
      "vendor": "IKEA"
    },
    "disabled": false,
    "endpoints": {
      "1": {
        "bindings": [
          {
            "cluster": "genPollCtrl",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "genPowerCfg",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          },
          {
            "cluster": "closuresWindowCovering",
            "target": {
              "endpoint": 1,
              "ieee_address": "0x00212effff07fc2e",
              "type": "endpoint"
            }
          }
        ],
        "clusters": {
          "input": [
            "genBasic",
            "genPowerCfg",
            "genIdentify",
            "genGroups",
            "genScenes",
            "genPollCtrl",
            "closuresWindowCovering",
            "touchlink",
            "64636"
          ],
          "output": [
            "genOta",
            "touchlink"
          ]
        },
        "configured_reportings": [
          {
            "attribute": "batteryPercentageRemaining",
            "cluster": "genPowerCfg",
            "maximum_report_interval": 62000,
            "minimum_report_interval": 3600,
            "reportable_change": 0
          },
          {
            "attribute": "currentPositionLiftPercentage",
            "cluster": "closuresWindowCovering",
            "maximum_report_interval": 62000,
            "minimum_report_interval": 1,
            "reportable_change": 1
          }
        ],
        "scenes": []
      }
    },
    "friendly_name": "Master blinds - closet side",
    "ieee_address": "0x84b4dbfffec40797",
    "interview_completed": true,
    "interviewing": false,
    "manufacturer": "IKEA of Sweden",
    "model_id": "TREDANSEN block-out cellul blind",
    "network_address": 65519,
    "power_source": "Battery",
    "software_build_id": "24.4.11",
    "supported": true,
    "type": "EndDevice"
  }
"""

unknownDeviceFixture :: String
unknownDeviceFixture = """
{
    "date_code": "20210824 22:26",
    "definition": {
      "description": "it's a WAT",
      "exposes": [
        {
          "access": 1,
          "description": "Remaining battery in %, can take up to 24 hours before reported. The only thing this does is suck power",
          "label": "Battery",
          "name": "battery",
          "property": "battery",
          "type": "numeric",
          "unit": "%",
          "value_max": 100,
          "value_min": 0
        }
      ]
    },
    "friendly_name": "WAT is this",
    "ieee_address": "0x84b4dbfffec40797",
    "manufacturer": "IKEA of Sweden",
    "model_id": "FUNUNNKENHANSENNNEN",
    "network_address": 65519,
    "power_source": "Battery",
    "supported": true,
    "type": "EndDevice"
  }
"""
