package com.teenthofabud.restaurant.solution.print.template.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class TemplateVo extends TOABBaseVo implements Comparable<TemplateVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String templateTypeId;
    @ToString.Include
    private String content;

    @Override
    public int compareTo(TemplateVo o) {
        return this.getName().compareTo(o.getName());
    }
}
