#include <minix/drivers.h>
#include <minix/chardriver.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <minix/ds.h>
#include "hello_stack.h"

/* Function prototypes for the hello driver. */
static ssize_t hello_stack_read(devminor_t minor, u64_t position, 
        endpoint_t endpt, cp_grant_id_t grant, size_t size, int flags, 
        cdev_id_t id); 
static int hello_stack_write(devminor_t minor, u64_t position, 
        endpoint_t endpt, cp_grant_id_t grant, size_t size, int flags, 
        cdev_id_t id);

/* SEF functions and variables. */
static void sef_local_startup(void);
static int sef_cb_init(int type, sef_init_info_t *info);
static int sef_cb_lu_state_save(int);
static void lu_state_restore(void);

/* Entry points to the hello driver. */
static struct chardriver hello_stack_tab =
{
    .cdr_read	= hello_stack_read,
    .cdr_write	= hello_stack_write,
};

/* Buffer and related data. */
static int stack_size, stack_filled;
static char *stack;

static ssize_t hello_stack_read(devminor_t UNUSED(minor),
        u64_t UNUSED(position), endpoint_t endpt, cp_grant_id_t grant,
        size_t size, int UNUSED(flags), cdev_id_t UNUSED(id))
{
    if (size > stack_filled)
        size = stack_filled;
    if (sys_safecopyto(endpt, grant, 0,
                       (vir_bytes) (stack + stack_filled - size), size) != OK)
        return -1;
    stack_filled -= size;
    if (4 * stack_filled <= stack_size && stack_size > 1) {
        stack_size /= 2;
        stack = realloc(stack, stack_size);
    }
    if (stack == NULL)
        return -1; 
    return size;
}

static ssize_t hello_stack_write(devminor_t UNUSED(minor),
        u64_t UNUSED(position), endpoint_t endpt, cp_grant_id_t grant,
        size_t size, int UNUSED(flags), cdev_id_t UNUSED(id))
{
    
    while (stack_size - stack_filled < size)
        stack_size *= 2;
    stack = realloc(stack, stack_size);
    if (stack == NULL)
        return -1;
    sys_safecopyfrom(endpt, grant, 0, (vir_bytes) (stack + stack_filled), size);
    stack_filled += size;
    return size;
}

static int sef_cb_lu_state_save(int UNUSED(state))
{
    ds_publish_u32("stack_size", stack_size, DSF_OVERWRITE);
    ds_publish_u32("stack_filled", stack_filled, DSF_OVERWRITE);
    ds_publish_str("stack", stack, DSF_OVERWRITE);
    free(stack);
    return OK;
}

static void lu_state_restore()
{
    u32_t value;

    ds_retrieve_u32("stack_size", &value);
    ds_delete_u32("stack_size");
    stack_size = (int) value;

    ds_retrieve_u32("stack_filled", &value);
    ds_delete_u32("stack_filled");
    stack_filled = (int) value;

    stack = malloc(sizeof(char) * stack_size);
    ds_retrieve_str("stack", stack, stack_filled);
    ds_delete_str("stack");
}

static void sef_local_startup()
{
    /* Register init callbacks. Use the same function for all event types */
    sef_setcb_init_fresh(sef_cb_init);
    sef_setcb_init_lu(sef_cb_init);
    sef_setcb_init_restart(sef_cb_init);

    /* Register live update callbacks. */
    /* - Agree to update immediately when LU is requested in a valid state. */
    sef_setcb_lu_prepare(sef_cb_lu_prepare_always_ready);
    /* - Support live update starting from any standard state. */
    sef_setcb_lu_state_isvalid(sef_cb_lu_state_isvalid_standard);
    /* - Register a custom routine to save the state. */
    sef_setcb_lu_state_save(sef_cb_lu_state_save);

    /* Let SEF perform startup. */
    sef_startup();
}

static int sef_cb_init(int type, sef_init_info_t *UNUSED(info))
{
    int do_announce_driver = TRUE;
    int i, j;

    switch(type) {
        case SEF_INIT_FRESH:
            /* Initialize buffer. */
            stack = malloc(sizeof(char) * DEVICE_SIZE);
            if (stack == NULL)
                return -1;
            for (i = 0; i < DEVICE_SIZE; i += 3)
                for (j = 0; i + j < DEVICE_SIZE && j < 3; ++j)
                    stack[i + j] = 'a' + j;
            stack_size = DEVICE_SIZE;
            stack_filled = DEVICE_SIZE;
        break;

        case SEF_INIT_LU:
            /* Restore the state. */
            lu_state_restore();
            do_announce_driver = FALSE;
        break;
    }
    if (do_announce_driver) {
        chardriver_announce();
    }
    return OK;
}

int main(void)
{
    sef_local_startup();
    chardriver_task(&hello_stack_tab);
    return OK;
}