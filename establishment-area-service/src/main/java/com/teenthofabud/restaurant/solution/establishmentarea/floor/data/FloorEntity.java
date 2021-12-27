package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "area_floor")
@EntityListeners(AuditingEntityListener.class)
@ToString(onlyExplicitlyIncluded = true)
public class FloorEntity extends TOABBaseEntity implements Comparable<FloorEntity>{
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	@ToString.Include
	private Long flrId;
	@ToString.Include
	@Column(name = "name")
	private String flrName;
	
	@OneToMany(mappedBy = "floor",fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	private List<KitchenEntity> kitchen = new ArrayList<KitchenEntity>();
	
	@OneToMany(mappedBy = "floor",fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	private List<TableEntity> table = new ArrayList<TableEntity>();

	@Override
	public int compareTo(FloorEntity o) {
		return this.getFlrId().compareTo(o.getFlrId());
	}
}
