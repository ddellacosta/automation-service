import {
  LitElement,
  html,
  css,
} from "https://unpkg.com/lit-element@2.0.1/lit-element.js?module";

class AutomationServiceCard extends LitElement {
  static get properties() {
    return {
      hass: {},
      config: {},
      statusLoaded: false,
    };
  }

  setConfig(config) {
    if (!config.entity) {
      throw new Error("You need to define an entity");
    }
    this.config = config;
  }

  static styles = css`
    .automation-list {
      display: flex;
      flex-direction: column;
    }

    .automation-entry {
      display: flex;
      flex-direction: column;
    }

    .loading {
      color: green;
    }
  `;

  runningAutomations(attributes) {
    if (attributes.runningAutomations?.length > 0) {
      return html`
        <div class="automation-list">
           ${attributes.runningAutomations.map(({name: name, startTime: startTime, devices: devices, groups: groups}) =>
             html`
               <div class="automation-entry">
                 <div>${name} - started at: ${startTime}</div>
                 ${devices.length > 0 ? html`<div>Devices: ${devices}</div>` : ``}
                 ${groups.length > 0 ? html`<div>Groups: ${groups}</div>` : ``}
               </div>
             `
           )}
         </div>
      `;
    } else {
      return html`<p class="loading">loading</p>`;
    }
  }

  scheduledAutomations(attributes) {
    if (attributes.scheduledAutomations?.length > 0) {
      return html`
        <div class="automation-list">
          ${attributes.scheduledAutomations.map(({jobId: jobId, job: job}) =>
            html`
              <div class="automation-entry">
                <div>${jobId} - Job: ${job}</div>
              </div>
            `
          )}
        </div>
      `;
    } else {
      return html`<p class="loading">loading</p>`;
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
      <ha-card header="automation-service">
        <h3>Running Automations</h3>
	${this.runningAutomations(attributes)}
        <h3>Scheduled Automations</h3>
	${this.scheduledAutomations(attributes)}
      </ha-card>
    `;
  }
}

customElements.define("automation-service-card", AutomationServiceCard);
