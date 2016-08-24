/**
 * 
 */
package org.volante.abm.netsens.institutions;


import java.util.Map;
import java.util.Set;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.volante.abm.agent.fr.FunctionalRole;
import org.volante.abm.agent.property.PropertyId;
import org.volante.abm.data.Cell;
import org.volante.abm.decision.pa.CraftyPa;
import org.volante.abm.decision.pa.PropertyProvidingPa;
import org.volante.abm.example.CellPropertyIds;
import org.volante.abm.example.measures.ConnectivityMeasure;
import org.volante.abm.institutions.AbstractCognitiveInstitution;
import org.volante.abm.institutions.pa.RestrictLandUsePa;
import org.volante.abm.output.tablecolumns.RestrictionNumber.RestrictionNumberProperties;
import org.volante.abm.param.RandomPa;


/**
 * Monitors: Connectivity within the region (see {@link ConnectivityMeasure}) via decision trigger Actions: Activate
 * restriction of allocation of particular FRs in a certain neighbourhood (see {@link RestrictLandUsePa}) Triggered:
 * Connectivity falling short of defined threshold (see {@link ConnectivityTrigger})
 * 
 * Kind of monitored connectivity is defined within the decision trigger, and the selected action then tells this
 * institution would to do, i.e. which restrictions to perform. The Similar-FRs-Map specifies at the same time
 * considered FR (map keys) and which FRs are considered similar when determining the number of similar FRs in the
 * neighbourhood (set as map value). Basically, the available actions need to define such a map.
 * 
 * Land use restriction can be limited by probability. Furthermore, the number of required similar FRs in the Moore
 * neighbourhood (8) can be defined.
 * 
 * @author Sascha Holzhauer
 * 
 */
public class FrConnectivityInstitution extends AbstractCognitiveInstitution {

	/**
	 * Restriction of an FR will take place with this probability (default: 1.0)
	 */
	@Element(required = false)
	protected double restrictionProbability = 1.0;

	@Element(required = false)
	protected double similarNeighboursRequired = 1;

	protected Map<FunctionalRole, Set<FunctionalRole>> relevantFrGroups = null;

	protected CraftyPa<?> pa;

	@Element(required = false)
	protected int minimumNeighbourhoodToCheck = 6;

	public enum AgentProperty implements PropertyId {
		CONNECTIVITY_THRESHOLD;
	}

	/**
	 * @param id
	 */
	public FrConnectivityInstitution(@Attribute(name = "id") String id) {
		super(id);
	}

	@Override
	public boolean isAllowed(FunctionalRole fr, Cell location) {
		if (relevantFrGroups != null && relevantFrGroups.containsKey(fr)) {
			Set<Cell> neighbours = region.getAdjacentCells(location);
			if (neighbours.size() == this.minimumNeighbourhoodToCheck) {
				int similarNeighbours = 0;
				for (Cell n : neighbours) {
					if (this.relevantFrGroups.get(fr).contains(n.getOwner().getFC().getFR())) {
						similarNeighbours++;
					}
				}
				if (similarNeighbours >= this.similarNeighboursRequired) {
					return true;
				} else {
					if (restrictionProbability > this.getRegion().getRandom().getURService()
					        .nextDouble(RandomPa.RANDOM_SEED_RUN.name())) {
						location.setObjectProperty(CellPropertyIds.RESTRICTED, fr.getSerialID());
						if (this.pa instanceof PropertyProvidingPa) {
							Map<PropertyId, Object> properties = ((PropertyProvidingPa) this.pa).getProperties();
							int number =
							        properties.containsKey(RestrictionNumberProperties.RESTRICTION_NUMBER) ? ((Integer) properties
							                .get(RestrictionNumberProperties.RESTRICTION_NUMBER)).intValue() : 0;
							properties.put(RestrictionNumberProperties.RESTRICTION_NUMBER, ++number);
						}
						return false;
					}
				}
			}
		}
		return true;
	}

	/**
	 * @param relevantFrGroups
	 * @param pa
	 */
	public void setRelevantFrSet(CraftyPa<?> pa, Map<FunctionalRole, Set<FunctionalRole>> relevantFrGroups) {
		this.relevantFrGroups = relevantFrGroups;
		this.pa = pa;
	}

	public String toString() {
		return "FrConnectivityInst (" + this.id + ")";
	}
}
