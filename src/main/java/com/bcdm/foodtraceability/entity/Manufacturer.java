package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 生产厂商信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Manufacturer implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 生产厂商ID
     */
    @TableId(type = IdType.AUTO)
    @NotNull(message = "当前生产厂商信息异常，请重新读取列表后再试", groups = {ModifyGroup.class, DeleteGroup.class})
    private Integer manufacturerId;

    /**
     * 企业ID
     */
    @NotNull(message = "当年公司信息异常，请重新登录后再试", groups = {DeleteGroup.class, ModifyGroup.class, CreateGroup.class})
    private Integer companyId;

    /**
     * 生产厂商名称
     */
    @Length(min = 1, max = 40, message = "生产厂商名称长度请控制在40以下", groups = {ModifyGroup.class, DeleteGroup.class})
    @NotBlank(message = "生产厂商名称不能为空", groups = {CreateGroup.class})
    private String manufacturerName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
