/**
 * 
 */
package org.volante.abm.netsens.institutions;


import org.simpleframework.xml.Element;
import org.volante.abm.agent.Agent;
import org.volante.abm.agent.fr.FunctionalRole;
import org.volante.abm.example.measures.ConnectivityMeasure;


/**
 * @author Sascha Holzhauer
 * 
 */
public class ServiceConnectivityTrigger extends ConnectivityTrigger {

	@Element(required = true)
	protected String serialFRole = null;

	protected FunctionalRole fRole = null;

	/**
	 * @param agent
	 * @return connectivity
	 */
	protected double getCurrentConnectivity(Agent agent) {

		if (this.fRole == null) {
			this.fRole = agent.getRegion().getFunctionalRoleMapByLabel().get(serialFRole);
		}
		return ConnectivityMeasure.getScore(agent.getRegion(), this.fRole);
	}
}
