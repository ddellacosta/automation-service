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
