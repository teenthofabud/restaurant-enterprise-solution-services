package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

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
@Table(name = "area_table")
@EntityListeners(AuditingEntityListener.class)
@ToString(onlyExplicitlyIncluded = true)
public class TableEntity extends TOABBaseEntity implements Comparable<TableEntity>{

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long tableId;
	
	@Column(name = "name")
	private String tableName;
	
	@Column(name = "description")
	private String description;

	@Column(name = "capacity")
	private String capacity;
	
	@ManyToOne(targetEntity = FloorEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	@JoinColumn(name = "floor_id")
	private FloorEntity floor;

	@Override
	public int compareTo(TableEntity o) {
		return this.getTableId().compareTo(o.getTableId());
	}
}
