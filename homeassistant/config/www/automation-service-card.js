import {
  LitElement,
  html,
  css,
} from "https://unpkg.com/lit-element@2.0.1/lit-element.js?module";

// straight lifted from https://stackoverflow.com/a/31415820
// because I suck at JS
function isLowerCase(str) {
  return str == str.toLowerCase() && str != str.toUpperCase();
}

class AutomationServiceCard extends LitElement {
  static get properties() {
    return {
      hass: {},
      config: {},
      statusLoaded: false,
      collapseState: {},
    };
  }

  setConfig(config) {
    if (!config.entity) {
      throw new Error("You need to define an entity");
    }
    this.config = config;
  }

  constructor() {
    super();
    this.collapseState = {};
  }

  toggleCollapse(autoName) {
    return (e) => {
      if (this.collapseState && typeof this.collapseState[autoName] !== undefined) {
        this.collapseState[autoName] = !this.collapseState[autoName]; 
      } else if (this.collapseState) {
        this.collapseState[autoName] = false;
      }
      // https://lit.dev/docs/components/properties/#mutating-properties
      this.requestUpdate();
    };
  }

  _renderDevices(devices) {
    return html`
      <div class="devices-header">^ Devices:</div>
      ${devices.map((deviceId) => html`<div class="device-id">${deviceId}</div>`)}
    `;
  }

  renderDevices(autoName, devices) {
    if (devices.length > 0) {
      return html`
        <div @click="${this.toggleCollapse(autoName)}" class="automation-info device-info">
          ${this.collapseState[autoName] ? this._renderDevices(devices) : html`<div class="devices-header">> Devices:</div>`}
        </div>
      `;
    } else {
      return "";
    }
  }

  renderGroups(autoName, groups) {
    if (groups.length > 0) {
      return html`
        <div class="automation-info group-info">
          <div class="groups-header">Groups: ${groups.map((groupId) => html`<span class="group-id">${groupId}</span>`)}</div>
        </div>
      `;
    } else {
      return "";
    }
  }

  runningAutomations(attributes) {
    if (attributes.runningAutomations?.length > 0) {
      return html`
        <div class="automation-list">
          <div class="automation-list-header">
           <span class="automation-name-header">name</span><span class="started-header">started</span>
          </div>

          ${attributes.runningAutomations.map(({name: name, startTime: startTime, devices: devices, groups: groups}) => {
	    const date = new Date(startTime);
            return html`
              <div class="automation-entry">
                <div class="automation-header running-automation-header">
                  <span class="${isLowerCase(name.slice(0,1)) ? "luascript-automation" : "system-automation"}">${name}</span>
                  <span class="automation-info start-time">${date.toLocaleString()}</span>
                </div>
                ${this.renderDevices(name, devices)}
                ${this.renderGroups(name, groups)}
              </div>
            `
	   })}
         </div>
      `;
    } else {
      return html`<p class="none">No running automations</p>`;
    }
  }

  scheduledAutomations(attributes) {
    if (attributes.scheduledAutomations?.length > 0) {
      return html`
        <div class="automation-list">
          <div class="scheduled-autos-header">
           <span class="job-id-header">job id</span>
           <span class="started-header">schedule (<a target="_blank" href="https://crontab.cronhub.io/">syntax</a>)</span>
          </div>

          ${attributes.scheduledAutomations.map(({schedule: schedule, jobId: jobId, job: job}) =>
            html`
              <div class="job-entry">
                <div class="job-header">
                  <span class="job-id">${jobId}</span>
                  <span class="schedule">${schedule}</span>
                </div>
		<div class="job">${job}</div>
                </div>
              </div>
            `
          )}
        </div>
      `;
    } else {
      return html`<p class="none">No scheduled automations</p>`;
    }
  }

  render() {
    if (!this.statusLoaded) {
      this.hass.callService('mqtt', 'publish', {
        topic: "automation-service/status",
        payload: "{\"homeassistant\": \"custom-card\"}"
      });
      this.statusLoaded = true;
    }

    const entityId = this.config.entity;
    const state = this.hass.states[entityId];
    const attributes = state ? state.attributes : {};

    return html`
      <ha-card>
        <div class="automation-service-container">
	  <h2>automation-service</h2>

          <h3>Running</h3>
	  ${this.runningAutomations(attributes)}

          <div class="scheduled-header">
            <h3>Scheduled</h3>
          </div>
	  ${this.scheduledAutomations(attributes)}
	</div>
      </ha-card>
    `;
  }

  static styles = css`
    .automation-service-container {
      margin-top: 2%;
      margin-left: 5%;
      width: 80%;
      margin-bottom: 4%;
    }

    .automation-service-container h2 {
      margin-bottom: 0;
    }

    .automation-service-container h3 {
      margin-bottom: 2%;
    }

    .automation-list {
      margin-left: 4%;
      display: flex;
      flex-direction: column;
    }

    .automation-list-header, .scheduled-autos-header {
      display: flex;
      flex-direction: row;
      justify-content: space-between;
    }

    .automation-entry {
      display: flex;
      flex-direction: column;
    }

    .none {
    }

    .luascript-automation {
      color: #44C544;
    }

    .system-automation {
      color: #428018;
    }

    .automation-info {
      margin-left: 2%;
    }

    .start-time {
      font-family: monospace;
      font-size: 0.9em;
    }

    .running-automation-header {
      display: flex;
      flex-direction: row; 
      justify-content: space-between;
    }

    .devices-header {
      cursor: pointer;
    }

    .devices-header:hover {
      color: #484CD0;
    }

    .device-id {
      margin-left: 3px;
      font-family: monospace;
      font-size: 0.9em;
    }

    .group-info {
    }

    .group-id {
      margin-left: 3px;
      font-family: monospace;
      font-size: 0.9em;
    }

    .scheduled-header {
    }

    .job-entry {
      margin-bottom: 3%;
    }

    .job-header {
      display: flex;
      flex-direction: row;
      justify-content: space-between;
    }

    .job-id {
      color: #44C544;
    }

    .job-details {
      margin-left: 2%;
    }

    .schedule {
      margin-right: 2%;
      font-family: monospace;
    }

    .job {
      margin-left: 2%;
      color: #428018;
    }
  `;
}

customElements.define("automation-service-card", AutomationServiceCard);
