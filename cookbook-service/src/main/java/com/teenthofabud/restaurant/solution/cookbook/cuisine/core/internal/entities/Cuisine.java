package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities;

import lombok.*;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class Cuisine {

    private Long id;
    private String name;
    private String description;
    private Boolean active;
    private LocalDateTime createdOn;
    private LocalDateTime modifiedOn;
    private Long createdBy;
    private Long modifiedBy;

}
