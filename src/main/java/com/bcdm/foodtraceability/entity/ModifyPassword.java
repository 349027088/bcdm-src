package com.bcdm.foodtraceability.entity;

import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

/**
 * <p>
 * 用户修改密码信息载体
 * </p>
 *
 * @author 王
 * @since 2022-01-30
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class ModifyPassword {

    /**
     * 需要修改密码的账号
     */
    @NotBlank(message = "账号不能为空", groups = {ModifyGroup.class})
    @Pattern(message = "账号只能为字母或者数字", regexp = "^[A-Za-z0-9]+$", groups = {ModifyGroup.class})
    @Length(max = 32, min = 6, message = "用户长度为6-32位", groups = {ModifyGroup.class})
    private String loginId;

    /**
     * 旧密码
     */
    @NotBlank(message = "密码不能为空", groups = {ModifyGroup.class})
    @Pattern(message = "密码只能为字母或者数字", regexp = "^[A-Za-z0-9]+$", groups = {ModifyGroup.class})
    @Length(max = 32, min = 6, message = "密码长度为6-32位", groups = {ModifyGroup.class})
    private String password;

    /**
     * 新密码
     */
    @NotBlank(message = "新密码不能为空", groups = {ModifyGroup.class})
    @Pattern(message = "新密码只能为字母或者数字", regexp = "^[A-Za-z0-9]+$", groups = {ModifyGroup.class})
    @Length(max = 32, min = 6, message = "新密码长度为6-32位", groups = {ModifyGroup.class})
    private String newPassword;

}
