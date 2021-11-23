package com.teenthofabud.restaurant.solution.menu.category.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "menu_category")
@EntityListeners(AuditingEntityListener.class)
public class CategoryEntity extends TOABBaseEntity implements Comparable<CategoryEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    /*@OneToMany(mappedBy = "account", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<AddressEntity> addresses;*/

    @Override
    public int compareTo(CategoryEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
