package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 商品类别信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class GoodsType implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 商品类别ID
     */
    @TableId(type = IdType.AUTO)
    @NotNull(message = "当前商品种类信息异常，请重新读取列表后再试", groups = {GetInfoGroup.class, ModifyGroup.class, DeleteGroup.class})
    private Integer goodsTypeId;

    /**
     * 企业ID
     */
    @NotNull(message = "当年公司信息异常，请重新登录后再试", groups = {GetInfoGroup.class, DeleteGroup.class, ModifyGroup.class, CreateGroup.class})
    private Integer companyId;

    /**
     * 商品类别名称
     */
    @Length(min = 1, max = 40, message = "商品种类名称长度请控制在40以下", groups = {CreateGroup.class,ModifyGroup.class})
    @NotBlank(message = "商品种类名称不能为空", groups = {CreateGroup.class,ModifyGroup.class})
    private String goodsTypeName;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
