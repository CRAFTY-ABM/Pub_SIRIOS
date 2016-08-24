/**
 * 
 */
package org.volante.abm.netsens.institutions;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.simpleframework.xml.ElementList;
import org.volante.abm.agent.Agent;
import org.volante.abm.agent.fr.FunctionalRole;
import org.volante.abm.example.measures.ConnectivityMeasure;


/**
 * @author Sascha Holzhauer
 * 
 */
public class SimilarConnectivityTrigger extends ConnectivityTrigger {

	@ElementList(inline = true, required = false, entry = "similarFrLabel", empty = false)
	public List<String> serialFrLabels = new ArrayList<>();

	protected Set<FunctionalRole> fRoles = null;

	/**
	 * @param agent
	 * @return connectivity
	 */
	protected double getCurrentConnectivity(Agent agent) {
		if (this.fRoles == null) {
			this.fRoles = new HashSet<>();
			for (String frLabel : this.serialFrLabels) {
				this.fRoles.add(agent.getRegion().getFunctionalRoleMapByLabel().get(frLabel));
			}
		}
		return ConnectivityMeasure.getScore(agent.getRegion(), this.fRoles);
	}
}
