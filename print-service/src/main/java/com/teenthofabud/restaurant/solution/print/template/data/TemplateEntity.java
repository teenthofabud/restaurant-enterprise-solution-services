package com.teenthofabud.restaurant.solution.print.template.data;

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
@Table(name = "print_template")
@EntityListeners(AuditingEntityListener.class)
public class TemplateEntity extends TOABBaseEntity implements Comparable<TemplateEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String description;
    @Column(name = "template_type_id")
    private String templateTypeId;
    private String content;

    @Override
    public int compareTo(TemplateEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
