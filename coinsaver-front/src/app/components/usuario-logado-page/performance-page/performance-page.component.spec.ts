import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PerformancePageComponent } from './performance-page.component';

describe('PerformancePageComponent', () => {
  let component: PerformancePageComponent;
  let fixture: ComponentFixture<PerformancePageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ PerformancePageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(PerformancePageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
