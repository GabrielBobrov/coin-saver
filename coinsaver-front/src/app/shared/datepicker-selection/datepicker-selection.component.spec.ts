import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DatepickerSelectionComponent } from './datepicker-selection.component';

describe('DatepickerComponent', () => {
  let component: DatepickerSelectionComponent;
  let fixture: ComponentFixture<DatepickerSelectionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DatepickerSelectionComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(DatepickerSelectionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
