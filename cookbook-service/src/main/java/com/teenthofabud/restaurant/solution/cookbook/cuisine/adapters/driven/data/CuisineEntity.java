package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "cuisine")
@EntityListeners(AuditingEntityListener.class)
public class CuisineEntity extends TOABBaseEntity implements Comparable<CuisineEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;

    @Override
    public int compareTo(CuisineEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
