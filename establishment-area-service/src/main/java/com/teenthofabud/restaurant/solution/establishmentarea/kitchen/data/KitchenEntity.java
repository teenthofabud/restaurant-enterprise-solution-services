package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import javax.persistence.*;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "area_kitchen")
@EntityListeners(AuditingEntityListener.class)
@ToString(onlyExplicitlyIncluded = true)
public class KitchenEntity extends TOABBaseEntity implements Comparable<KitchenEntity>{

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long kitchenId;

	@Column(name = "name")
	private String kitchenName;

	@Column(name = "description")
	private String description;

	@ManyToOne(targetEntity = FloorEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	@JoinColumn(name = "floor_id")
	private FloorEntity floor;

	@Override
	public int compareTo(KitchenEntity o) {
		return this.getKitchenId().compareTo(o.getKitchenId());
	}
}
