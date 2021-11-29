package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "kitchen")
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
