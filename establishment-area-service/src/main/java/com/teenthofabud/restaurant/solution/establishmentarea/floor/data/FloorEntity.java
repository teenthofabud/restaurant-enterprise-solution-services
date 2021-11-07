package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;

import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "floor")
public class FloorEntity extends TOABBaseEntity{
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private int flrId;
	@Column(name = "name")
	private String flrName;
	
	@OneToMany(mappedBy = "floor",fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private List<KitchenEntity> kitchen = new ArrayList<KitchenEntity>();
	
	@OneToMany(mappedBy = "floor",fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private List<TableEntity> table = new ArrayList<TableEntity>();
}
