/**
 * 
 */
package org.volante.abm.netsens.institutions;


import org.apache.log4j.Logger;
import org.volante.abm.agent.Agent;
import org.volante.abm.agent.bt.LaraBehaviouralComponent;
import org.volante.abm.agent.property.PropertyId;
import org.volante.abm.decision.trigger.AbstractDecisionTrigger;
import org.volante.abm.decision.trigger.InformedTrigger;
import org.volante.abm.example.measures.ConnectivityMeasure;

import de.cesr.lara.components.decision.LaraDecisionConfiguration;
import de.cesr.lara.components.model.impl.LModel;


/**
 * NOTE: There can be only one {@link ConnectivityTrigger} per institution (as multiple would set the same property
 * onnectivityTriggerIds.LAST_RECORD_TICK and)!
 * 
 * @author Sascha Holzhauer
 */
public class ConnectivityTrigger extends AbstractDecisionTrigger {

	/**
	 * Logger
	 */
	static private Logger logger = Logger.getLogger(ConnectivityTrigger.class);

	enum ConnectivityTriggerIds implements PropertyId {
		LAST_RECORD_TICK;
	}

	/**
	 * @see org.volante.abm.decision.trigger.DecisionTrigger#check(org.volante.abm.agent.Agent)
	 */
	@Override
	public boolean check(Agent agent) {
		double currentConnectivity = getCurrentConnectivity(agent);
		boolean triggered = false;

		// <- LOGGING
		if (logger.isDebugEnabled()) {
			logger.debug(agent
			        + "> Checking Connectivity ("
			        + agent.getRegion()
			        + ")"
			        + (agent.isProvided(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD) ? "(current: "
			                + currentConnectivity + " - target: "
			                + agent.getProperty(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD) + ") "
			                : "(not CONNECTIVITY_THRESHOLD available)") + "...");
		}
		// LOGGING ->

		if (agent.isProvided(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD)
		        && agent.isProvided(ConnectivityTriggerIds.LAST_RECORD_TICK)
		        && agent.getRegion().getRinfo().getSchedule().getCurrentTick() > agent
		                .getProperty(ConnectivityTriggerIds.LAST_RECORD_TICK)) {
			if (agent.getProperty(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD) > currentConnectivity) {
				LaraDecisionConfiguration dConfig =
				        LModel.getModel(agent.getRegion()).getDecisionConfigRegistry().get(this.dcId);

				((LaraBehaviouralComponent) agent.getBC()).subscribeOnce(
				        dConfig,
				        new InformedTrigger(this, currentConnectivity + "["
				                + agent.getProperty(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD)
				                + "]"));
				triggered = true;

				// <- LOGGING
				logger.info(agent + "> Triggered regional institutional action (current connectivity: "
				        + currentConnectivity + " - target: "
				        + agent.getProperty(FrConnectivityInstitution.AgentProperty.CONNECTIVITY_THRESHOLD) + ")");
				// LOGGING ->
			}
		}
		agent.setProperty(ConnectivityTriggerIds.LAST_RECORD_TICK, (double) agent.getRegion().getRinfo().getSchedule()
		        .getCurrentTick());

		return triggered;
	}

	/**
	 * @param agent
	 * @return connectivity
	 */
	protected double getCurrentConnectivity(Agent agent) {
		return ConnectivityMeasure.getScore(agent.getRegion());
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "ConnectivityTrg";
	}
}
