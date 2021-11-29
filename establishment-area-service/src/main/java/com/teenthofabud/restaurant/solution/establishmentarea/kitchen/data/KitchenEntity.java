package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import javax.persistence.*;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "kitchen")
@EntityListeners(AuditingEntityListener.class)
@ToString(onlyExplicitlyIncluded = true)
public class KitchenEntity extends TOABBaseEntity {

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

}
